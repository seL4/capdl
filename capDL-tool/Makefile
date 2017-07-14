#
# Copyright 2017, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(DATA61_BSD)
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
	stack exec $(TARGET) -- -t $*.parse -x $*.xml -d $*.dot $< || rm -f $*.parse $*.dot $*.xml
	dot -Tpng $*.dot -o $*.png
	@diff -b $*.parse $*.right
	@stack exec $(TARGET) -- -t $*.parse.x $*.parse > /dev/null
	@diff $*.parse $*.parse.x  || (echo "Self parse failed"; exit 1)
	which xmllint && xmllint --noout --dtdvalid ./capdl.dtd $*.xml

.PHONY: sandbox
sandbox: stack.yaml capDL-tool.cabal
	stack setup
	stack build --only-dependencies

$(TARGET): stack.yaml capDL-tool.cabal Setup.hs Main.hs CapDL/*.hs
	CONFIG_CAPDL_LOADER_MAX_IRQS=$(CONFIG_CAPDL_LOADER_MAX_IRQS) stack build

.PHONY: install
install: $(TARGET)
	stack install

.PHONY: clean
clean:
	rm -f $(TARGET)

.PHONY: testclean
testclean:
	rm -f *.parse *.parse.x *.dot *.xml *.thy *.png *.out
