PHONY: all
all: 
	-rm foo.o foo.hi foo.o_modpak
	# ----
	(cd ~/work/ghc-whole-program-compiler-project/external-stg/ && cabal build ext-stg-mlir)
	~/work/ghc-whole-program-compiler-project/ghc-wpc/_build/stage1/bin/ghc foo.hs
	~/work/ghc-whole-program-compiler-project/external-stg/dist-newstyle/build/x86_64-linux/ghc-8.8.3/external-stg-0.1.0.1/x/ext-stg/build/ext-stg/ext-stg show foo.o_modpak > foo.stg
	~/work/ghc-whole-program-compiler-project/external-stg/dist-newstyle/build/x86_64-linux/ghc-8.8.3/external-stg-0.1.0.1/x/ext-stg-mlir/build/ext-stg-mlir/ext-stg-mlir show foo.o_modpak
	# ----
	# -rm smallpt.o smallpt.hi smallpt.o_modpak
	# (cd ~/work/ghc-whole-program-compiler-project/external-stg/ && cabal build ext-stg-mlir)
	# ~/work/ghc-whole-program-compiler-project/ghc-wpc/_build/stage1/bin/ghc smallpt.hs
	# ~/work/ghc-whole-program-compiler-project/external-stg/dist-newstyle/build/x86_64-linux/ghc-8.8.3/external-stg-0.1.0.1/x/ext-stg/build/ext-stg/ext-stg show smallpt.o_modpak > smallpt.stg
	# ~/work/ghc-whole-program-compiler-project/external-stg/dist-newstyle/build/x86_64-linux/ghc-8.8.3/external-stg-0.1.0.1/x/ext-stg-mlir/build/ext-stg-mlir/ext-stg-mlir show smallpt.o_modpak

