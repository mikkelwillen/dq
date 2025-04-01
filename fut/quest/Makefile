QUESTDIR=~/gits/QuEST
COMPUTECAPABILITY=80

.PHONY: all
all: qft/qft.exe ghz/ghz.exe grover/grover.exe
	$(MAKE) run_qft run_ghz run_grover

qft/qft.exe: qft/qft.c
	(cd qft; cmake $(QUESTDIR) -DOUTPUT_EXE=qft.exe -DGPUACCELERATED=0 -DMULTITHREADED=1 -DUSER_SOURCE=qft.c)
	(cd qft; make -B)

qft/qft_cuda.exe: qft/qft.c
	(cd qft; cmake $(QUESTDIR) -DOUTPUT_EXE=qft_cuda.exe -DGPUACCELERATED=1 -DGPU_COMPUTE_CAPABILITY=$(COMPUTECAPABILITY) -DUSER_SOURCE=qft.c)
	(cd qft; make -B)

ghz/ghz.exe: ghz/ghz.c
	(cd ghz; cmake $(QUESTDIR) -DOUTPUT_EXE=ghz.exe -DGPUACCELERATED=0 -DMULTITHREADED=1 -DUSER_SOURCE=ghz.c)
	(cd ghz; make -B)

ghz/ghz_cuda.exe: ghz/ghz.c
	(cd ghz; cmake $(QUESTDIR) -DOUTPUT_EXE=ghz_cuda.exe -DGPUACCELERATED=1 -DGPU_COMPUTE_CAPABILITY=$(COMPUTECAPABILITY) -DUSER_SOURCE=ghz.c)
	(cd ghz; make -B)

grover/grover.exe: grover/grover.c
	(cd grover; cmake $(QUESTDIR) -DOUTPUT_EXE=grover.exe -DGPUACCELERATED=0 -DMULTITHREADED=1 -DUSER_SOURCE=grover.c)
	(cd grover; make -B)

grover/grover_cuda.exe: grover/grover.c
	(cd grover; cmake $(QUESTDIR) -DOUTPUT_EXE=grover_cuda.exe -DGPUACCELERATED=1 -DGPU_COMPUTE_CAPABILITY=$(COMPUTECAPABILITY) -DUSER_SOURCE=grover.c)
	(cd grover; make -B)

circuits/grover_%.qsim: grover/grover.exe
	@mkdir -p circuits
	grover/grover.exe -l $* 2> $@ >/dev/null

circuits/qft_%.qsim: qft/qft.exe
	@mkdir -p circuits
	qft/qft.exe -l $* 2> $@ >/dev/null

GROVER_CIRCUITS=circuits/grover_8.qsim circuits/grover_10.qsim circuits/grover_12.qsim circuits/grover_14.qsim circuits/grover_16.qsim circuits/grover_18.qsim
QFT_CIRCUITS=circuits/qft_14.qsim circuits/qft_15.qsim circuits/qft_16.qsim circuits/qft_17.qsim circuits/qft_18.qsim circuits/qft_19.qsim  circuits/qft_20.qsim

.PHONY: circuits
circuits: $(GROVER_CIRCUITS) $(QFT_CIRCUITS)

QSIMGEN=-l
#QSIMGEN=

.PHONY: run_grover
run_grover: grover/grover.exe
	for n in 8 10 12 14 16 18 ; do \
           grover/grover.exe -t $(QSIMGEN) $$n 2> grover/grover_$$n.qsim | grep Elapsed ; \
        done

.PHONY: run_grover_cuda
run_grover_cuda: grover/grover_cuda.exe
	@grover/grover_cuda.exe -t 8 | grep Elapsed
	@grover/grover_cuda.exe -t 10 | grep Elapsed
	@grover/grover_cuda.exe -t 12 | grep Elapsed
	@grover/grover_cuda.exe -t 14 | grep Elapsed
	@grover/grover_cuda.exe -t 16 | grep Elapsed
	@grover/grover_cuda.exe -t 18 | grep Elapsed

.PHONY: run_ghz
run_ghz: ghz/ghz.exe
	for n in 21 22 23 24 25 26 27 ; do \
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
