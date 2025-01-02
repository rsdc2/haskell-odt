# Haskell ODT


## Overview
Haskell-ODT provides an interface in Haskell for reading and writing Open Document Format `.odt` files.


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

## Dependencies and licenses

- [bytestring](https://hackage.haskell.org/package/bytestring-0.12.1.0/docs/Data-ByteString.html): BSD-style license
- [containers](https://hackage.haskell.org/package/containers): BSD-3
- [text](https://hackage.haskell.org/package/text-2.1.2/docs/Data-Text.html): BSD-style
- [directory](https://hackage.haskell.org/package/directory): BSD-3
- [xml](https://hackage.haskell.org/package/xml): BSD-3
- [xml-conduit](https://hackage.haskell.org/package/xml-conduit): MIT
- [zip-archive](https://hackage.haskell.org/package/zip-archive-0.4.3.2/docs/Codec-Archive-Zip.html): BSD-3
- [extra](https://hackage.haskell.org/package/extra): BSD-3
- [HUnit](https://hackage.haskell.org/package/HUnit): BSD-3