PROG	:= semdoc
ARGS	:= src/Text/Semdoc.lhs spec.md

run: dist/build/$(PROG)/$(PROG)
	$< $(ARGS)
	cat spec.md

dist/build/$(PROG)/$(PROG): dist/setup-config $(shell find src -type f)
	cabal build

dist/setup-config: $(PROG).cabal
	cabal configure --disable-profiling
