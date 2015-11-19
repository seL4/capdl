#
# Copyright 2014, NICTA
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(NICTA_BSD)
#

all: capDL.pdf

capDL.pdf: capDL.tex
	pdflatex capDL
	pdflatex capDL

clean:
	rm -f *.log
	rm -f *.aux
	rm -f *.out

realclean: clean
	rm -f capDL.pdf
