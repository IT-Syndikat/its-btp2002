#!/usr/bin/env runghc

{-# OPTIONS_GHC -threaded #-}

{-# LANGUAGE ViewPatterns, OverloadedStrings, ScopedTypeVariables #-}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char
import Data.Bits
import Data.Word
import Data.String
import System.IO
import System.Exit
import System.Environment
import System.Directory
import System.FilePath
import System.Posix.Temp
import System.Process
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Control.Exception
import Numeric

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["server", file] -> server file >> return ()
    _ -> do
      ss <- sequence (go args)
      mapM_ (either BS.putStr putStrWithEncoding) ss

server f = do
    hdl <- openFile f ReadWriteMode
    hSetBuffering hdl NoBuffering
    hSetBuffering stdin NoBuffering
    hSetBuffering stderr NoBuffering
    t1 <- createThread $ forever $ BS.hPut hdl =<< h' (BS.hGetSome stdin 1024)
    t2 <- createThread $ forever $ do
             s <- h' (BS.hGet hdl 5)
             let [b0,b1,b2,b3,b4] = BS.unpack s
             hPutStrLn stderr $ show $ parseASB (b1,b2,b3,b4)
             BS.hPut stdout s
    waitThread t1
    waitThread t2
  where
    h' fn = do
      s <- fn
      if s == BS.empty
        then exitSuccess
        else return s

data Thread a = Thread ThreadId (MVar (Either SomeException a))
createThread :: IO a -> IO (Thread a)
waitThread :: Thread a -> IO a

createThread ma = do
    mv <- newEmptyMVar
    tid <- forkIO $
             putMVar mv =<< ((Right <$> ma) `catch` handler)
    return $ Thread tid mv
  where
    handler se@(SomeException _) = return (Left se)

waitThread (Thread tid mv) = do
  x <- readMVar mv
  case x of
    Left  se -> throwIO se
    Right a  -> return a

data ASB = ASB {
      asbDrawer :: Bool
    , asbOnline :: Bool
    , asbCover  :: Bool
    , asbFeed   :: Bool

    , asbErrCutter :: Bool
    , asbErrUnrecv :: Bool
    , asbErrRecv   :: Bool

    , asbPaperNearEnd :: W4
    , asbPaperEnd     :: W4
    } deriving (Eq, Ord, Read, Show)

data W4 = W0 | W1 | W2 | W3 deriving (Enum, Eq, Ord, Read, Show)

parseASB :: (Word8, Word8, Word8, Word8) -> ASB
parseASB (b0,b1,b2,b3) =
  ASB
    (bit 2 b0 > 0)
    (bit 3 b0 > 0)
    (bit 5 b0 > 0)
    (bit 6 b0 > 0)

    (bit 3 b1 > 0)
    (bit 5 b1 > 0)
    (bit 6 b1 > 0)

    (toEnum $ fromIntegral $ bit 0 b2 .|. bit 1 b2)
    (toEnum $ fromIntegral $ bit 2 b2 .|. bit 3 b2)
 where
   bit i b = (1 `shiftL` i) .&. b

-- idle status report: 05 14 00 00 0f


data StringWithEncoding = SWE String String deriving (Eq, Ord, Read, Show)
putStrWithEncoding (SWE enc str) = do
  hSetEncoding stdout =<< mkTextEncoding enc
  putStr str

go :: [String] -> [IO (Either ByteString StringWithEncoding)]
go ("init":r) =
    return (Left "\ESC@")                                            : go r
go ("cut":r)        =
    return (Left "\GSVB\NUL")                                        : go r
go ("EAN13":sz:n:r) =
    return (Left $ barcode_EAN13 (read sz) (fromString n))           : go r
go ("PDF417":n:r)   =
    return (Left $ barcode_PDF417 (fromString n))                    : go r
go ("feed":r)       =
    return (Left "\n")                                               : go r
go ("line-CP852":w:r) =
    map return [ Left "\ESCt\18", Right (SWE "CP852" w), Left "\n" ] ++ go r
go ("line":w:r)     =
    return (Right (SWE "CP437" $ w ++ "\n"))                         : go r
go ("sep":r)        =
   return (Right $ SWE "CP437" $ replicate 42 '-')                   : go r
go ("sep_line":c:r) =
   return (Right $ SWE "CP437" $ take 42 $ cycle c )                 : go r
go ("size":sz:r)    =
    return (Left (BS8.pack [ '\GS', '!', chr (read sz) ] ))          : go r
go ("bitmap" : (bl -> xsc) : (bl -> ysc) : f : r)  =
    (Left . bitmap (xsc, ysc) . parsePBM  <$> readFile f)            : go r
go ("latex":tex:r)  =
    (Left . bitmap (False, False) <$> texToPbm tex)                  : go r
go ("page_enter":r) =
    return (Left "\ESCL")                                            : go r
go ("page_exit":r) =
    return (Left "\ESCS")                                            : go r
go ("page_print":r) =
    return (Left "\ESC\255")                                         : go r
go ("just_left":r) =
    return (Left $ setJustification JustLeft)                        : go r
go ("just_center":r) =
    return (Left $ setJustification JustCenter)                      : go r
go ("just_right":r) =
    return (Left $ setJustification JustLeft)                        : go r
go ("disable_buttons":r) =
    return (Left "\ESCc5\255")                                       : go r
go ("enable_asb":r) =
    return (Left "\GSa\255")                                         : go r
go ("disable_asb":r) =
    return (Left "\GSa\0")                                           : go r
go [] = []
go r = error $ "Unmatched: " ++ show r

bl c = case c of "0" -> False; "1" -> True

barcode_EAN13 :: Word8 -> ByteString -> ByteString
barcode_EAN13 sz num |    BS.length num `elem` [12, 13]
                       && BS8.all isDigit num
                       && sz <= 120 && sz > 0 =
    BS.concat [ BS8.pack [ '\GS', 'h', chr (fromIntegral sz)
                         , '\GS', 'H', '\2'
                         , '\GS', 'k', 'C', chr (BS.length num)
                         ]
              , num
              ]

barcode_PDF417 :: ByteString -> ByteString
barcode_PDF417 num | BS.length num > 0 && BS.length num < 255 =
    BS.concat [ BS8.pack [ '\GS', 'k', 'K', chr (BS.length num) ], num ]

data Justification = JustLeft | JustCenter | JustRight

setJustification :: Justification -> ByteString
setJustification j = BS8.pack [ '\ESC', 'a', pp j ]
  where
    pp JustLeft   = '\0'
    pp JustCenter = '\1'
    pp JustRight  = '\2'

data PBM = PBM Int Int [[Word8]] deriving (Show)

parsePBM :: String -> PBM
parsePBM s = let "P1":hdr:r = mapMaybe dropComments $ lines s
                 [x, y] = map read $ words hdr
             in PBM x y $ reshape x $ go $ map (mapMaybe dropSpaces) r
  where
    dropComments ('#':_) = Nothing
    dropComments l = Just l

    dropSpaces ' ' = Nothing
    dropSpaces c   = Just c

    go (l:ls) = map (\c -> case c of '0' -> 0; '1' -> 1) l : go ls
    go [] = []

reshape :: Int -> [[Word8]] -> [[Word8]]
reshape x ls = gogo $ concat ls
  where
    gogo ps =
        case splitAt x ps of
          (xs, []) -> xs : []
          (xs,ys) -> xs : gogo ys

bitmap :: (Bool, Bool) -> PBM -> ByteString
bitmap sc (PBM x' y' bm) = BS.concat [
    BS8.pack [ '\GS', 'v', '0', chr m, chr xL, chr xH, chr yL, chr yH ], dat]
  where
    m = case sc of
          (False, False) -> 0
          (True , False) -> 1
          (False, True ) -> 2
          (True , True ) -> 3
    xL =  x .&. 0xff
    xH = (x `shiftR` 8) .&. 0xff
    yL =  y .&. 0xff
    yH = (y `shiftR` 8) .&. 0xff
    x = (x' + 7) `div` 8
    y = y'
    dat = BS8.pack $ map (chr . fromIntegral) $ concat $ map packBits bm

packBits :: [Word8] -> [Word8]
packBits [] = []
packBits r@(_:_:_:_:_:_:_:_:_) = let
    (bs, rs) = splitAt 8 r
  in
    foldr (.|.) 0 (map (uncurry shiftL) (bs `zip` [7,6..0])) : packBits rs
packBits r =
    packBits $ take 8 (r ++ [0,0..])

texToPbm :: String -> IO PBM
texToPbm tex = do
    (fn, hdl) <- mkstemps "/tmp/BTPhs" ".tex"
    hPutStr hdl (tpl tex)
    hClose hdl
    _ <- readCreateProcess (proc "xelatex" [fn])
                               { cwd = Just (takeDirectory fn) } ""

    _ <- readCreateProcess (proc "pdfcrop" [fn -<.> "pdf"])
                               { cwd = Just (takeDirectory fn) } ""
    let fn_pdf = replaceFileName fn $ (takeBaseName fn ++ "-crop") <.> "pdf"
        fn_pbm = fn -<.> "pbm"
    _ <- readCreateProcess
           (proc "convert" [ "-density", "203x180", "-compress", "none"
                           , fn_pdf, fn_pbm ])
             { cwd = Just (takeDirectory fn) } ""
    parsePBM <$> readFile fn_pbm
  where
    tpl src =
      "\\documentclass[xetex]{scrartcl}\n\
      \\\usepackage{amsmath}\n\
      \\\usepackage[ top=0mm\
                  \, bottom=0mm\
                  \, left=0mm\
                  \, right=0mm\
                  \, paperwidth=80mm\
                  \, paperheight=1000mm]{geometry}\n\
      \\\begin{document}\n\
      \\\thispagestyle{empty}\n"
       ++ src ++
      "\n\\end{document}\n"
