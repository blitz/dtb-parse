# dtb-parse

[![Hackage version](https://img.shields.io/hackage/v/dtb-parse.svg?label=Hackage)](https://hackage.haskell.org/package/dtb-parse) [![Stackage version](https://www.stackage.org/package/dtb-parse/badge/lts?label=Stackage)](https://www.stackage.org/package/dtb-parse) ![Build Status](https://github.com/blitz/dtb-parse/workflows/build/badge.svg)

The `dtb-parse` package provides `Data.Dtb`, a module to parse
flattened device trees (DTB). These files are usually used to describe
the hardware on embedded platforms.

`dtb-parse` is a pure Haskell implementation of DTB parsing. It uses
no code from
[libfdt](https://git.kernel.org/pub/scm/utils/dtc/dtc.git).


## Building the Library

The most convenient way to build the library is using
[Stack](https://haskellstack.org):

```sh
% stack build
% stack test
```

## Usage

To parse a device tree and dump it to the console, you can do the
following:

```Haskell
import qualified Data.ByteString as B
import           Data.Dtb

main = do
    dtbData <- B.readFile "some.dtb"
    putStrLn $ show $ parseDtb dtbData
```
