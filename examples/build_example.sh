#!/bin/sh

ARG=$1
shift

ghc --make -fforce-recomp $ARG -fplugin=Supercompilation.Plugin -ddump-simpl -ddump-to-file $*
