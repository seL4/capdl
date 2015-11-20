#
# Copyright 2014, NICTA
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(NICTA_BSD)
#

TARGET=parse-capDL

.PHONY: all
all: $(TARGET)

.PHONY: tests
tests: example-arm.parse example-ia32.parse hello-dump.parse

# The size of the IRQ array we emit (if using --code) needs to match the size
# expected by the initialiser. If we're building within a project, try to
# retrieve this from the current configuration.
-include .config
ifndef CONFIG_CAPDL_LOADER_MAX_IRQS
    CONFIG_CAPDL_LOADER_MAX_IRQS = 256
endif

%.parse: %.cdl %.right $(TARGET)
	./$(TARGET) -t $*.parse -x $*.xml -d $*.dot $< || rm -f $*.parse $*.dot $*.xml
	dot -Tpng $*.dot -o $*.png
	@diff $*.parse $*.right
	@./$(TARGET) -t $*.parse.x $*.parse > /dev/null
	@diff $*.parse $*.parse.x  || (echo "Self parse failed"; exit 1)
	which xmllint && xmllint --noout --dtdvalid ./capdl.dtd $*.xml

.PHONY: sandbox
sandbox:
	cabal sandbox init
	cabal install --dependencies-only

.PHONY: userdeps
userdeps:
	cabal install --user --dependencies-only

$(TARGET): capDL-tool.cabal Setup.hs Main.hs CapDL/*.hs
	export CONFIG_CAPDL_LOADER_MAX_IRQS=$(CONFIG_CAPDL_LOADER_MAX_IRQS) && cabal configure && cabal build && \
	cp dist/build/parse-capDL/parse-capDL .

.PHONY: clean
clean:
	cabal clean
	rm -f $(TARGET)

.PHONY: testclean
testclean: 
	rm -f *.parse *.parse.x *.dot *.xml *.thy *.png *.out
