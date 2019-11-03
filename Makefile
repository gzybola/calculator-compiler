all:
	happy -gca src/ParInstant.y
	alex -g src/LexInstant.x
	ghc --make -isrc src/LLVMTest.hs -o insc_llvm
	ghc --make -isrc src/JVMTest.hs -o insc_jvm
	-rm -f src/*.o src/*.log src/*.aux src/*.hi src/*.dvi
clean:
	-rm insc_jvm insc_llvm
distclean: clean
	-rm -f DocInstant.* LexInstant.* ParInstant.* LayoutInstant.* SkelInstant.* PrintInstant.* TestInstant.* AbsInstant.* TestInstant ErrM.* SharedString.* ComposOp.* Instant.dtd XMLInstant.* Makefile*
	

