# QuEST benchmarks for comparisons

Each QuEST benchmark is located in subdirectories. Each of the examples
includes [quest_util.h](`quest_util.h`) and assumes that QuEST sources
are installed (e.g., at `~/gits/QuEST`; path may be changed in `Makefile`).

Just execute `make clean all` to run the benchmarks.

If the option `-l` is passed to an executable (e.g., as in
`ghz/ghz.exe -l 5`), [QSIM code](https://github.com/quantumlib/qsim/blob/master/docs/input_format.md) will be printed to `<stderr>`.

### Benchmarks:

- [ghz/ghz.c](`ghz.c`)
- [grover/grover.c](`grover.c`)
