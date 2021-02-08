#!/usr/bin/env zsh
set -e
set -o xtrace
GHCWPC=/home/bollu/work/ghc-whole-program-compiler-project/ghc-wpc/_build/stage1/bin/ghc
EXTSTG=/home/bollu/work/ghc-whole-program-compiler-project/external-stg/dist-newstyle/build/x86_64-linux/ghc-8.8.3/external-stg-0.1.0.1/x/ext-stg/build/ext-stg/ext-stg
EXTSTGMLIR=/home/bollu/work/ghc-whole-program-compiler-project/external-stg/dist-newstyle/build/x86_64-linux/ghc-8.8.3/external-stg-0.1.0.1/x/ext-stg-mlir/build/ext-stg-mlir/ext-stg-mlir
FILEPATH=$(dirname $1)
NAME=$(basename $1 .hs)

(cd ${FILEPATH} && ${GHCWPC} -O0 ${NAME}.hs -o ${NAME}.out)
(cd ${FILEPATH} && ${EXTSTG} show ${NAME}.o_modpak > ${NAME}.stg)
(cd ${FILEPATH} && ${EXTSTGMLIR} show ${NAME}.o_modpak | tee ${NAME}.mlir)
