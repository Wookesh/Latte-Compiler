all:
	cd src; bnfc Latte.cf
	cd src; happy -gca ParLatte.y
	cd src; alex -g LexLatte.x
	cd src; ghc --make Main.hs -o ../latc_x86_64
clean:
	cd src; rm -f *.log *.aux *.hi *.o *.dvi
	cd src; rm -f DocLatte.ps
distclean: clean
	cd src; rm -f DocLatte.* LexLatte.* ParLatte.* LayoutLatte.* SkelLatte.* PrintLatte.* TestLatte.* AbsLatte.* TestLatte ErrM.* SharedString.* Latte.dtd XMLLatte.*

