QUESTDIR=~/gits/QuEST
COMPUTECAPABILITY=80

.PHONY: all
all: qft/qft.exe ghz/ghz.exe grover/grover.exe
	$(MAKE) run_qft run_ghz run_grover

qft/qft.exe: qft/qft.c
	(cd qft; cmake $(QUESTDIR) -DOUTPUT_EXE=qft.exe -DGPUACCELERATED=0 -DMULTITHREADED=1 -DUSER_SOURCE=qft.c)
	(cd qft; make -B)

qft/qft_cuda.exe: qft/qft.c
	(cd qft; cmake $(QUESTDIR) -DOUTPUT_EXE=qft_cuda.exe -DGPUACCELERATED=1  -DMULTITHREADED=0 -DGPU_COMPUTE_CAPABILITY=$(COMPUTECAPABILITY) -DUSER_SOURCE=qft.c)
	(cd qft; make -B)

ghz/ghz.exe: ghz/ghz.c
	(cd ghz; cmake $(QUESTDIR) -DOUTPUT_EXE=ghz.exe -DGPUACCELERATED=0 -DMULTITHREADED=1 -DUSER_SOURCE=ghz.c)
	(cd ghz; make -B)

ghz/ghz_cuda.exe: ghz/ghz.c
	(cd ghz; cmake $(QUESTDIR) -DOUTPUT_EXE=ghz_cuda.exe -DGPUACCELERATED=1 -DMULTITHREADED=0 -DGPU_COMPUTE_CAPABILITY=$(COMPUTECAPABILITY) -DUSER_SOURCE=ghz.c)
	(cd ghz; make -B)

grover/grover.exe: grover/grover.c
	(cd grover; cmake $(QUESTDIR) -DOUTPUT_EXE=grover.exe -DGPUACCELERATED=0 -DMULTITHREADED=1 -DUSER_SOURCE=grover.c)
	(cd grover; make -B)

grover/grover_cuda.exe: grover/grover.c
	(cd grover; cmake $(QUESTDIR) -DOUTPUT_EXE=grover_cuda.exe -DGPUACCELERATED=1 -DMULTITHREADED=0 -DGPU_COMPUTE_CAPABILITY=$(COMPUTECAPABILITY) -DUSER_SOURCE=grover.c)
	(cd grover; make -B)

GROVER_NS=8 10 12 14 16 18
GHZ_NS=21 22 23 24 25 26 27
QFT_NS=14 15 16 17 18 19 20

.PHONY: run_grover
run_grover: grover/grover.exe
	for n in $(GROVER_NS) ; do \
           grover/grover.exe -t $(QSIMGEN) $$n 2> grover/grover_$$n.qsim | grep Elapsed ; \
        done

.PHONY: run_grover_cuda
run_grover_cuda: grover/grover_cuda.exe
	for n in $(GROVER_NS) ; do \
           grover/grover_cuda.exe -t $(QSIMGEN) $$n 2> grover/grover_$$n.qsim | grep Elapsed ; \
        done

.PHONY: run_ghz
run_ghz: ghz/ghz.exe
	for n in $(GHZ_NS) ; do \
	  ghz/ghz.exe -t $(QSIMGEN) $$n 2> ghz/ghz_$$n.qsim | grep Elapsed ; \
        done

.PHONY: run_ghz_cuda
run_ghz_cuda: ghz/ghz_cuda.exe
	for n in $(GHZ_NS) ; do \
	  ghz/ghz_cuda.exe -t $(QSIMGEN) $$n 2> ghz/ghz_$$n.qsim | grep Elapsed ; \
        done

.PHONY: run_qft
run_qft: qft/qft.exe
	for n in $(QFT_NS) ; do \
	  qft/qft.exe -t $(QSIMGEN) $$n 2> qft/qft_$$n.qsim | grep Elapsed ; \
        done

.PHONY: run_qft_cuda
run_qft_cuda: qft/qft_cuda.exe
	for n in $(QFT_NS) ; do \
	  qft/qft_cuda.exe -t $(QSIMGEN) $$n 2> qft/qft_$$n.qsim | grep Elapsed ; \
        done

.PHONY: clean
clean:
	rm -rf */*.exe */Makefile *~ */*~ */CMake* */QuEST */*.cmake */*.qsim
