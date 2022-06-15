#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

TARGET=parse-capDL

.PHONY: all
all: $(TARGET)

.PHONY: tests
tests: example-arm.test-OK example-ia32.test-OK hello-dump.test-OK cap-dist-elf-simpleserver.test-OK camkes-adder-arm.test-OK camkes-adder-arm.test-thy-OK

%.parse %.dot %.xml: %.cdl $(TARGET)
	stack exec $(TARGET) -- -t $*.parse -x $*.xml -d $*.dot $< || rm -f $*.parse $*.dot $*.xml

%.test-OK: %.parse %.dot %.xml %.right
	@diff -b $*.parse $*.right
	@stack exec $(TARGET) -- -t $*.parse.x $*.parse > /dev/null
	@diff $*.parse $*.parse.x  || (echo "Self parse failed"; exit 1)
	which xmllint && xmllint --noout --dtdvalid ./capdl.dtd $*.xml
	dot -Tpng $*.dot -o $*.png || true # don't fail the test if graph layout is too hard
	echo placeholder for Makefile >$*.test-OK

camkes-adder-arm.test-thy-OK: $(TARGET) camkes-adder-arm.cdl camkes-adder-arm.thy.right
	@stack exec $(TARGET) -- -i camkes-adder-arm.thy --object-sizes=camkes-adder-arm.object_sizes.yaml camkes-adder-arm.cdl
	@diff -b camkes-adder-arm.thy camkes-adder-arm.thy.right || (echo "Isabelle output differs"; exit 1)
	echo placeholder for Makefile >camkes-adder-arm.test-thy-OK

# This tells GNU Make that *.parse files should not be considered as intermediate files
# for the *.test-OK targets, so that Make won't delete them.
# We can't use a wildcard here because it's not implemented in Make; see
# https://stackoverflow.com/questions/27090032
.SECONDARY: example-arm.parse example-arm.dot example-arm.xml \
  example-ia32.parse example-ia32.dot example-ia32.xml \
  hello-dump.parse hello-dump.dot hello-dump.xml \
  cap-dist-elf-simpleserver.parse cap-dist-elf-simpleserver.dot cap-dist-elf-simpleserver.xml \
  camkes-adder-arm.parse camkes-adder-arm.dot camkes-adder-arm.xml camkes-adder-arm.thy

.PHONY: sandbox
sandbox:
	stack setup
	stack build --only-dependencies

# `stack install` copies the binary out of the build dir
# --fast compiles with out any optimisations and should be more appropriate
# for current capDL-tool usage behavior.
.PHONY: $(TARGET)
$(TARGET):
	stack build --fast
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
	rm -f *.parse *.parse.x *.dot *.xml *.thy *.png *.out *-OK
