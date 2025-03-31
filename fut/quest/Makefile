QUESTDIR=~/gits/QuEST

.PHONY: all
all: qft/qft.exe ghz/ghz.exe grover/grover.exe
	$(MAKE) run_qft run_ghz run_grover

qft/qft.exe: qft/qft.c
	(cd qft; cmake $(QUESTDIR) -DOUTPUT_EXE=qft.exe -DUSER_SOURCE=qft.c)
	(cd qft; make)

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

.PHONY: run_ghz
run_ghz: ghz/ghz.exe
	@ghz/ghz.exe -t 20 | grep Elapsed
	@ghz/ghz.exe -t 22 | grep Elapsed
	@ghz/ghz.exe -t 24 | grep Elapsed
	@ghz/ghz.exe -t 26 | grep Elapsed
	@ghz/ghz.exe -t 28 | grep Elapsed
	@ghz/ghz.exe -t 30 | grep Elapsed

.PHONY: run_qft
run_qft: qft/qft.exe
	@qft/qft.exe -t 14 | grep Elapsed
	@qft/qft.exe -t 16 | grep Elapsed
	@qft/qft.exe -t 18 | grep Elapsed
	@qft/qft.exe -t 20 | grep Elapsed
	@qft/qft.exe -t 22 | grep Elapsed
	@qft/qft.exe -t 24 | grep Elapsed

.PHONY: clean
clean:
	rm -rf */*.exe */Makefile *~ */*~ */CMake* */QuEST */*.cmake
