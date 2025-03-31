QUESTDIR=~/gits/QuEST

.PHONY: all
all: ghz/ghz.exe grover/grover.exe
	$(MAKE) run_ghz run_grover

ghz/ghz.exe: ghz/ghz.c
	(cd ghz; cmake $(QUESTDIR) -DOUTPUT_EXE=ghz.exe -DUSER_SOURCE=ghz.c)
	(cd ghz; make)

grover/grover.exe: grover/grover.c
	(cd grover; cmake $(QUESTDIR) -DOUTPUT_EXE=grover.exe -DUSER_SOURCE=grover.c)
	(cd grover; make)

.PHONY: run_grover
run_grover: grover/grover.exe
	@grover/grover.exe -t 8 | grep Elapsed
	@grover/grover.exe -t 10 | grep Elapsed
	@grover/grover.exe -t 12 | grep Elapsed
	@grover/grover.exe -t 14 | grep Elapsed
	@grover/grover.exe -t 16 | grep Elapsed
	@grover/grover.exe -t 18 | grep Elapsed

.PHONY: run_grover
run_ghz: ghz/ghz.exe
	@ghz/ghz.exe -t 20 | grep Elapsed
	@ghz/ghz.exe -t 22 | grep Elapsed
	@ghz/ghz.exe -t 24 | grep Elapsed
	@ghz/ghz.exe -t 26 | grep Elapsed
	@ghz/ghz.exe -t 28 | grep Elapsed
	@ghz/ghz.exe -t 30 | grep Elapsed

.PHONY: clean
clean:
	rm -rf */*.exe */Makefile *~ */*~ */CMake* */QuEST */*.cmake
