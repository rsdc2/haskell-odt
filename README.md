# Haskell ODT


## Overview

Haskell-ODT provides an interface in Haskell for reading and writing Open Document Format `.odt` files.

The program is set up to enable the user to compose their own `.odt` files using Haskell syntax. 

## Installing and building

So far I have only tested this process on Linux (Ubuntu), not Windows or MacOS. You can install with [Stack](https://docs.haskellstack.org/en/stable/). I installed Haskell and Stack with [GHCup](https://www.haskell.org/ghcup/). 

### 1. Clone and `cd` into the repo

```
$ git clone https://github.com/rsdc2/haskell-odt
$ cd haskell-odt
```

### 2. Build

```
$ stack build
```

### 3. Run the tests
```
$ stack test
```

### 4. Run the Main app
```
$ stack run
```

## Example usage


## Key concepts

## Acknowledgements 

The main inspiration for this project was the [HaTeX](https://gitlab.com/daniel-casanueva/haskell/HaTeX) project, which provides an interface in Haskell for composing and otherwise analysing LaTeX documents. 

## Dependencies and licenses

- [bytestring](https://hackage.haskell.org/package/bytestring-0.12.1.0/docs/Data-ByteString.html): BSD-style
- [containers](https://hackage.haskell.org/package/containers): [BSD-3](https://hackage.haskell.org/package/containers-0.7/src/LICENSE)
- [directory](https://hackage.haskell.org/package/directory): [BSD-3](https://hackage.haskell.org/package/directory-1.3.9.0/src/LICENSE)
- [extra](https://hackage.haskell.org/package/extra): [BSD-3](https://hackage.haskell.org/package/extra-1.8/src/LICENSE)
- [mtl](https://hackage.haskell.org/package/mtl): [BSD-3](https://hackage.haskell.org/package/mtl-2.3.1/src/LICENSE)
- [text](https://hackage.haskell.org/package/text-2.1.2/docs/Data-Text.html): BSD-style
- [time](https://hackage.haskell.org/package/time): [BSD-2](https://hackage.haskell.org/package/time-1.14/src/LICENSE)
- [xml](https://hackage.haskell.org/package/xml): [BSD-3](https://hackage.haskell.org/package/xml-1.3.14/src/LICENSE)
- [xml-conduit](https://hackage.haskell.org/package/xml-conduit): [MIT](https://hackage.haskell.org/package/xml-conduit-1.10.0.0/src/LICENSE)
- [zip-archive](https://hackage.haskell.org/package/zip-archive-0.4.3.2/docs/Codec-Archive-Zip.html): BSD-3
- [HUnit](https://hackage.haskell.org/package/HUnit): [BSD-3](https://hackage.haskell.org/package/HUnit-1.6.2.0/src/LICENSE)