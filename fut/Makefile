
lib:
	futhark pkg sync

.PHONY: test
test: lib
	futhark test qsim_test.fut ex1.fut grover.fut

.PHONY: clean
clean:
	rm -rf *.expected *.actual *~ *.c ex1 qsim_test grover

.PHONY: realclean
realclean: clean
	rm -rf lib
