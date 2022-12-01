FLAGS = -fPIC -O2 -g
PACKAGES = parsec "./lib.hs"
HFLAGS = -package $(PACKAGES) $(FLAGS) -threaded -rtsopts -v0 -XNoImplicitPrelude
DONE1 = $(wildcard [1-9][ab].hs)
DONE2 = $(wildcard [12][0-9][ab].hs)

all: $(sort $(DONE1:%.hs=%.output)) $(sort $(DONE2:%.hs=%.output))
	@for output in $^; do /bin/echo -n "$${output/.output}: "; cat "$$output" && echo "\n"; done

clean:
	@rm -f -- [0-9][0-9][ab] [0-9][ab] *.o *.hi *.so *.a

distclean: clean
	@rm -f -- *.output

%a.output: %a %.input
	@./$< +RTS -N12 < $*.input > $@

%b.output: %b %.input
	@./$< +RTS -N12 < $*.input > $@

%:: %.hs
	@ghc $(HFLAGS) -o $@ $^
	@rm -f -- $*.hi $*.o

.PRECIOUS: %
.PHONY: all clean distclean
