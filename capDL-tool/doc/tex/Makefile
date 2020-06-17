#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

all: capDL.pdf

GEN=capDL_generated.tex

capDL.pdf: capDL.tex $(GEN)
	pdflatex capDL
	pdflatex capDL

$(GEN): ../capDL.md
	pandoc $< -o $@

clean:
	rm -f *.log
	rm -f *.aux
	rm -f *.out
	rm -f $(GEN)

realclean: clean
	rm -f capDL.pdf
