all:
	happy -gca ParLatte.y
	alex -g LexLatte.x
	latex DocLatte.tex; dvips DocLatte.dvi -o DocLatte.ps
	ghc --make TestLatte.hs -o TestLatte
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocLatte.ps
distclean: clean
	-rm -f DocLatte.* LexLatte.* ParLatte.* LayoutLatte.* SkelLatte.* PrintLatte.* TestLatte.* AbsLatte.* TestLatte ErrM.* SharedString.* Latte.dtd XMLLatte.* Makefile*

