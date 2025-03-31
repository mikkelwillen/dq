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

QSIMGEN=-l
#QSIMGEN=

.PHONY: run_grover
run_grover: grover/grover.exe
	for n in 8 10 12 14 16 18 ; do \
           grover/grover.exe -t $(QSIMGEN) $$n 2> grover/grover_$$n.qsim | grep Elapsed ; \
        done

.PHONY: run_ghz
run_ghz: ghz/ghz.exe
	for n in 20 22 24 26 28 30 ; do \
           ghz/ghz.exe -t $(QSIMGEN) $$n 2> ghz/ghz_$$n.qsim | grep Elapsed ; \
        done

.PHONY: run_qft
run_qft: qft/qft.exe
	for n in 14 16 18 20 22 24 ; do \
           qft/qft.exe -t $(QSIMGEN) $$n 2> qft/qft_$$n.qsim | grep Elapsed ; \
        done

.PHONY: clean
clean:
	rm -rf */*.exe */Makefile *~ */*~ */CMake* */QuEST */*.cmake */*.qsim
