
MLCOMP ?= mlton

../../lib:
	$(MAKE) -C ../.. lib

grover.exe: grover.mlb ../../lib
	$(MLCOMP) -output $@ $<

.PHONY: clean
clean:
	rm -rf *~ MLB *.exe
