
lib:
	futhark pkg sync

.PHONY: test
test: lib
	futhark test dqfut_test.fut ex1.fut grover.fut ghz.fut
	$(MAKE) -C qsim test

.PHONY: clean
clean:
	rm -rf *.expected *.actual *~ *.c ex1 dqfut_test grover ghz *.multicore.data

.PHONY: realclean
realclean: clean
	rm -rf lib
