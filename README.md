# dq - The DIKU Quantum Simulation Framework [![CI](https://github.com/diku-dk/dq/workflows/CI/badge.svg)](https://github.com/diku-dk/dq/actions)

This repository contains the sources for __dq__ - the DIKU Quantum Simulation
Framework. The source code is structured to separate the concerns of specifying
and drawing circuits from providing a semantics and an evaluation framework for
circuits.

Multiple state-vector simulators are available ranging from semantics-based
simulators, based on the unitaries that circuits denote, over more efficient
simulators where gates work directly as strided operations over a global state
vector, to specialising simulators that work by generating optimised
data-parallel code to be executed on GPUs. These GPU simulators are based on a
Futhark quantum simulation library, which is also part of the sources.

## Library Structure

- `fut/`: [Futhark](http://futhark-lang.org) data-parallel gate library. The
  library can be used on its own for writing quantum algorithms or it can be
  used as a target library for high-level languages.

- `src/`: Modules for specifying and drawing circuits, modules for giving the
  semantics for circuits, modules for executing circuits and for compiling
  circuits into data-parallel Futhark code.

- `examples/`: Example quantum algorithms.

## Dependencies

The framework builds with both the [MLKit](https://github.com/melsman/mlkit) and
[MLton](http://mlton.org/) compilers and uses
[`smlpkg`](https://github.com/diku-dk/smlpkg) to fetch relevant libraries,
including [`sml-complex`](https://github.com/diku-dk/sml-complex) and
[`sml-matrix`](https://github.com/diku-dk/sml-matrix), libraries for easily
working with complex numbers and matrices in Standard ML.

On macos, you may install `smlpkg` and `mlkit` using `brew install smlpkg
mlkit`, assuming you have Homebrew installed.

On Linux, you may download binaries from the respective repositories.

As mentioned, the framework also supports the generation of
[Futhark](http://futhark-lang.org) [1] code for simulating circuits. Futhark is
available for most platforms. For installation details, see
[http://futhark-lang.org](http://futhark-lang.org).

## Compiling the Source Code

To compile the source code and run the tests, just execute `make test` in the
source directory. The default is to use `mlkit` as a compiler. If you must use
MLton, execute `MLCOMP=mlton make test` instead.

## Simple Example Run

Here is a simple example run:
```
$ cd src
$ mlkit quantum_ex1.mlb
...
bash-3.2$ ./run
Circuit for c = (I ** H oo CX oo Z ** Z oo CX oo I ** H) ** I oo I ** SW oo CX ** Y:
              .---.
----------*---| Z |---*-----------------*----
          |   '---'   |                 |
          |           |                 |
  .---. .-+-. .---. .-+-. .---.       .-+-.
--| H |-| X |-| Z |-| X |-| H |-.   .-| X |--
  '---' '---' '---' '---' '---'  \ /  '---'
                                  /
                                 / \  .---.
--------------------------------'   '-| Y |--
                                      '---'
Semantics of c:
~i  0  0  0  0  0  0  0
 0  0  i  0  0  0  0  0
 0 ~i  0  0  0  0  0  0
 0  0  0  i  0  0  0  0
 0  0  0  0  0 ~i  0  0
 0  0  0  0  0  0  0  i
 0  0  0  0 ~i  0  0  0
 0  0  0  0  0  0  i  0
Result distribution when evaluating c on |101> :
|000> : 0
|001> : 0
|010> : 0
|011> : 0
|100> : 1
|101> : 0
|110> : 0
|111> : 0
```

## Larger Examples

1. The framework contains an implementation of Grover's algorithm written using
   Standard ML combinators (`examples/grover/grover.sml`) and an implementation
   written directly in Futhark using Futhark state-vector transformations
   (`fut/grover.fut`).

## Literature

[1] Martin Elsman, Troels Henriksen, and Cosmin Oancea. Parallel Programming in
Futhark. Edition 0.8. Department of Computer Science, University of
Copenhagen. Edition Nov
22, 2023. [latest-pdf](https://readthedocs.org/projects/futhark-book/downloads/pdf/latest/).
