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
tests: example-arm.test-OK example-ia32.test-OK hello-dump.test-OK

%.parse %.dot %.xml: %.cdl $(TARGET)
	stack exec $(TARGET) -- -t $*.parse -x $*.xml -d $*.dot $< || rm -f $*.parse $*.dot $*.xml

%.test-OK: %.parse %.dot %.xml %.right
	@diff -b $*.parse $*.right
	@stack exec $(TARGET) -- -t $*.parse.x $*.parse > /dev/null
	@diff $*.parse $*.parse.x  || (echo "Self parse failed"; exit 1)
	which xmllint && xmllint --noout --dtdvalid ./capdl.dtd $*.xml
	dot -Tpng $*.dot -o $*.png || true # don't fail the test if graph layout is too hard
	echo placeholder for Makefile >$*.test-OK

# This tells GNU Make that *.parse files should not be considered as intermediate files
# for the *.test-OK targets, so that Make won't delete them.
# We can't use a wildcard here because it's not implemented in Make; see
# https://stackoverflow.com/questions/27090032
.SECONDARY: example-arm.parse example-arm.dot example-arm.xml example-ia32.parse example-ia32.dot example-ia32.xml hello-dump.parse hello-dump.dot hello-dump.xml

.PHONY: sandbox
sandbox:
	stack setup
	stack build --only-dependencies

# `stack install` copies the binary out of the build dir
.PHONY: $(TARGET)
$(TARGET):
	stack build
	stack install

.PHONY: install
# Deprecated. This used to be `stack install` but did not install
# to any meaningful PATH location
install:
	echo >&2 'capDL-tool/Makefile: install target no longer exists'
	false

.PHONY: clean
clean:
	rm -f $(TARGET)

.PHONY: testclean
testclean:
	rm -f *.parse *.parse.x *.dot *.xml *.thy *.png *.out
