all:
	ghc --make -isrc/ src/Compiler.hs -o latc_llvm
clean:
	-rm -f src/*.hi src/*.o
	-rm -f latc_llvm

