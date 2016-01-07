all:
	bnfc Latte.cf
	happy -gca ParLatte.y
	alex -g LexLatte.x
	ghc --make Main.hs -o latc
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocLatte.ps
distclean: clean
	-rm -f DocLatte.* LexLatte.* ParLatte.* LayoutLatte.* SkelLatte.* PrintLatte.* TestLatte.* AbsLatte.* TestLatte ErrM.* SharedString.* Latte.dtd XMLLatte.*

