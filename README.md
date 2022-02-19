# Emu
Emu is a simple systems language ala C. Emu is built from Haskell, C++, and LLVM.

## How far along is Emu?
Emu is "complete" in the sense that I've implemented all the features I set out to implement when I started the project. If I feel like adding more (a good module system, comptime arguments, etc.), I will, but no promises.

- [x] Basic CLI interface
- [x] Parser & AST
- [x] Semantic analysis
- [x] Haskell to C++ marshalling
- [x] LLVM code generation
- [x] Fleshed out command line interface
- [x] Comptime evaluator
- [x] Full language semantic checking
- [x] Core language feature-set
- [ ] Good documentation

## Installation
To build emu, you will need GHC, Cabal, g++, and LLVM installed. You should install g++ and LLVM through your system's package manager. To install GHC & Cabal, I recommend you use [GHCup](https://www.haskell.org/ghcup/). To build the project, just run 
```
cabal build
``` 
in the project root. The built executable will be at ```dist-newstyle/build/<OS>/<ghc-version>/<emu-version>/x/emu/build/emu/emu```.
To build the provided tests / examples, you will need GNU make. Navigate to the ```tests/c-tests``` directory and run
```
make all
```
There is an Emacs major mode in the ```emacs``` directory. To use it, add
```emacs-lisp
(autoload 'emu-mode "emu-mode" nil t)
```
to your ```init.el``` file.
