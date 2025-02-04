SMLPKG = smlpkg

.PHONY: all
all: lib
	$(MAKE) -C src all

.PHONY: test
test: lib
	$(MAKE) -C src test
	$(MAKE) -C fut test

.PHONY: clean
clean:
	rm -rf *~
	$(MAKE) -C src clean
	$(MAKE) -C fut clean
	$(MAKE) -C examples/grover clean

.PHONY: realclean
realclean:
	$(MAKE) clean
	rm -rf lib
	$(MAKE) -C fut realclean

lib:
	$(SMLPKG) sync
