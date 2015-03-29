#
# Makefile for an IEEE style report
#

LU_FLAVOURS=PDF

# remember that the first target is the universal target
# HFLAGS = -O3 -prof -osuf p_o -hisuf p_hi
HFLAGS = -O3 -rtsopts -with-rtsopts "-M7500M -K1500M"
# runtime options +RTS -xc

all:	run main.pdf

WHAT = all
run:
	ghc $(HFLAGS) main.lhs
	time nice ./main $(WHAT)

main.pdf : main.tex 

# showhs just displays the haskell code from a lhs source

showhs:
	awk '/begin{code}/,/end{code}/' main.lhs | \
	grep -v '{code}'

clean::
	-rm -f main main.hi main.o *.txt *.pdf *~

include LaTeX.mk




