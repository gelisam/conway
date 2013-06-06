all: test
.PHONY: all deps test clean clobber

deps: src/find_deps.rb
	$< > $@

include deps

bin/%: src/%.hs
	rm -rf crumbs/Main.o
	mkdir -p crumbs $(dir $@)
	ghc $(CCFLAGS) -o $@ -icrumbs:src -odir crumbs -hidir crumbs --make $<
bin/%: tests/%.hs
	rm -rf crumbs/Main.o
	mkdir -p crumbs $(dir $@)
	ghc $(CCFLAGS) -o $@ -icrumbs:src:tests -odir crumbs -hidir crumbs --make $<


#test: $(patsubst tests/%.expected,proofs/%.proof,$(shell find tests -name '*.expected'))
#	-@echo '*** ALL TESTS OK ***'

test: bin/test
	./bin/test

proofs/%.proof: bin/% tests/%.expected
	$(MAKE) $<
	mkdir -p $(dir $@)
	$< | diff - $(patsubst proofs/%.proof,tests/%.expected,$@)
	touch $@


clean:
	rm -rf crumbs proofs

clobber: clean
	rm -rf bin deps
