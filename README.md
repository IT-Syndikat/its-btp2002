BTP2002NP Thermal Receipt Printer Driver
========================================

Requirements
------------

You need to have the Glasgow Haskell Compiler installed, no additional libraries
are needed and no explicit compilation is required.

On Debian-like systems this can be done with:

```
$ apt-get install ghc
```

Usage
-----

The basic mode of operation is to just pipe the output of the `driver.hs`
program into the printer device, like so:

```
$ ./driver.hs ARGS... > /dev/usb/lpX
```

where `lpX` is the device node of your printer.

### Printing text

```
$ ./driver.hs init line "Hello world" feed cut
```


### Printing bitmaps

```
$ ./driver.hs init bitmap 0 0 bitmaps/logo2013_name.pbm feed cut
```

### Printing LaTeX


```
$ ./driver.hs init just_center latex "x + y^2 = 0" feed cut
```

From a file:

```
$ ./driver.hs init just_center latex "$(cat foo.tex)" feed cut
```
