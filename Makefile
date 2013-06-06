all: test
.PHONY: all deps test clean clobber

deps: src/find_deps.rb
	$< > $@

include deps

bin/%: src/%.hs
	rm -rf crumbs/Main.o
	mkdir -p crumbs $(dir $@)
	ghc $(CCFLAGS) -o $@ -icrumbs:src -odir crumbs -hidir crumbs --make $<

test: bin/test
	./bin/test


clean:
	rm -rf crumbs proofs

clobber: clean
	rm -rf bin deps
