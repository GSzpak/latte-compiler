all:
	ghc --make -isrc/ src/Compiler.hs -o latc
clean:
	-rm -f src/*.hi src/*.o
	-rm -f latc

