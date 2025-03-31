
lib:
	futhark pkg sync

.PHONY: test
test: lib
	futhark test dqfut_test.fut ex1.fut grover.fut ghz.fut qft.fut fusion.fut
	$(MAKE) -C qsim test

.PHONY: clean
clean:
	rm -rf *.expected *.actual *~ *.c ex1 dqfut_test grover ghz qft fusion *.multicore.data
	$(MAKE) -C qsim clean
	$(MAKE) -C quest clean

BENCHBACKEND?=c
.PHONY: bench
bench:
	futhark bench -r 3 --backend=$(BENCHBACKEND) ghz.fut grover.fut qft.fut

.PHONY: realclean
realclean: clean
	rm -rf lib
