QSIMDIR=~/gits/qsim
QSIM_FILES=test3.qsim circuit_q24.qsim
CXX=g++
CXXFLAGS=-O3
NVCC=nvcc
NVCCFLAGS=$(CXXFLAGS)

.PHONY: all
all: test3.fut

.PHONY: test
test: test3.c.res
	cat $^

.PHONY: clean
clean:
	rm -rf *~ *.c *.fut test3 circuit_q24 *.exe *.out *.res

%.fut: %.qsim ../../src/qsim2fut
	../../src/qsim2fut -dqfut ../dqfut $< > $@

%.c.exe: %.fut
	futhark c -o $@ $<

%.c.out: %.c.exe
	cat $*.in | ./$< > $@
	cat $@

%.res: %.out
	@(diff -aq $< $<.ok > /dev/null 2>&1; \
         if [ $$? -eq 0 ]; then \
             echo "OK:  $*" > $@ \
         ; else \
             if [ -e $<.ok ]; then \
                echo "ERR: $* - file $< differs from $<.ok"; \
                echo "ERR: $* - file $< differs from $<.ok" > $@ \
             ; else \
                echo "ERR: $* - file $<.ok does not exist"; \
                echo "ERR: $* - file $<.ok does not exist" > $@ \
             ; fi \
             ; exit 1 \
         ;fi)

../../src/qsim2fut:
	$(MAKE) -C ../../src qsim2fut

qsim_cpu_seq: qsim_cpu.cpp
	$(CXX) -o $@ $< -I $(QSIMDIR) $(CXXFLAGS)

qsim_cpu_par: qsim_cpu.cpp
	$(CXX) -o $@ $< -I $(QSIMDIR) $(CXXFLAGS) -fopenmp

qsim_cuda: qsim_cuda.cu
	$(NVCC) -o $@ $< -I $(QSIMDIR) $(NVCCFLAGS)
