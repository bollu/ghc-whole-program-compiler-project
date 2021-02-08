#!/usr/bin/env zsh
GHCWPC=/home/bollu/work/ghc-whole-program-compiler-project/ghc-wpc/_build/stage1/bin/ghc
EXTSTG=/home/bollu/work/ghc-whole-program-compiler-project/external-stg/dist-newstyle/build/x86_64-linux/ghc-8.8.3/external-stg-0.1.0.1/x/ext-stg/build/ext-stg/ext-stg
EXTSTGMLIR=/home/bollu/work/ghc-whole-program-compiler-project/external-stg/dist-newstyle/build/x86_64-linux/ghc-8.8.3/external-stg-0.1.0.1/x/ext-stg-mlir/build/ext-stg-mlir/ext-stg-mlir
NAME=$(basename $1 .hs)

${GHCWPC} ${NAME}.hs
${EXTSTG} show ${NAME}.o_modpak > ${NAME}.stg
${EXTSTGMLIR} show ${NAME}.o_modpak | tee ${NAME}.mlir
