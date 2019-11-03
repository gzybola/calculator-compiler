all:
	happy -gca ParInstant.y
	alex -g LexInstant.x
	ghc --make LLVM.hs -o insc_llvm
	ghc --make JVM.hs -o insc_jvm

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocInstant.* LexInstant.* ParInstant.* LayoutInstant.* SkelInstant.* PrintInstant.* TestInstant.* AbsInstant.* TestInstant ErrM.* SharedString.* ComposOp.* Instant.dtd XMLInstant.* Makefile*
	

