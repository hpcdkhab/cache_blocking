#!/bin/sh

#GNU oder Intel
COMPILER=$1
#Converting all letters to capital:
COMPILER=${COMPILER^^}
ARCH=HASWELL
if [ "${COMPILER}" == "INTEL" ];  then
  echo "Compiler Intel"
elif [ "${COMPILER}" == "GNU" ];  then
  echo "Compiler GNU"
else
  echo "Error: no compiler;"
  echo "use: ./comp GNU|Intel"
  exit 1
fi

if [ "${COMPILER}" == "INTEL" ];  then
  module load compiler/intel/parallel_studio_xe_2019_update5
  set -x
  CC=icc
  FF=ifort
  LINKER=ifort
  OPT_FORT="-O3 -march=core-avx2 -qopenmp  -W1 -align array64byte "
  OPT_ASM_FORT="-O3 -march=core-avx2 -qopenmp  -W1  -S -fsource-asm  -qopt-report -qopt-report-routine:laplacian_simple_block3d -align array64byte"
  OPT_ASM_CC_OPT="-W -vec-threshold0  -march=core-avx2 -S -fsource-asm  -qopt-report -fno-alias"
  OPT_LINKER="-O3 -qopenmp"
  OPT_CC="-O3 -march=core-avx2 -W -qopenmp -fno-alias -qopt-report"

  #Für Haswell, Skylake und ROME verwende -march=core-avx2
else
  module load compiler/gnu
  CC=gcc #x86_64-redhat-linuxcc
  FF=gfortran
  LINKER=gfortran
  OPT_FORT="-O3    -march=haswell -fopenmp -Wall -ffree-line-length-1024"
  OPT_ASM_FORT="-O3  -march=haswell -fopenmp -Wall  -fopt-info-vec -fopt-info-vec-optimized -S -fverbose-asm -ffree-line-length-1024"
  OPT_ASM_CC_OPT="-W -march=haswell -fopt-info-vec -fopt-info-vec-optimized -S -fverbose-asm -fno-strict-aliasing"
  OPT_LINKER="-O3   -fopenmp"
  OPT_CC="-O3 -fno-strict-aliasing -fopt-info-vec -fopt-info-vec-optimized  -Wall"
fi

set -x

rm laplacian3d.o mod_laplacian3d.o mod_laplacian_block3d.o  mod_inparam.o mod_cmdline.o mod_types.o timer.o mod_timer.o cc_laplacian_block3d.o mod_miscelanos.o  laplacian3d_${COMPILER}_${ARCH} -rf
rm mod_types.mod mod_timer.mod mod_laplacian3d.mod mod_inparam.mod mod_cmdline.mod mod_laplacian_block3d.mod mod_miscelanos.o -rf
rm mod_laplacian_block3d.S.${COMPILER}_${ARCH}  cc_laplacian_block3d.S.${COMPILER}_${ARCH} -rf

${CC} ${OPT_CC} timer.c -c
${CC} ${OPT_CC} cc_laplacian_block3d.c -c
${FF} ${OPT_FORT} mod_types.f90 -c
${FF} ${OPT_FORT} mod_profile.f90 -c
${FF} ${OPT_FORT} mod_cmdline.f90 -c
${FF} ${OPT_FORT} mod_inparam.f90 -c
${FF} ${OPT_FORT} mod_timer.f90 -c
${FF} ${OPT_FORT} mod_miscelanos.f90 -c
${FF} ${OPT_FORT} mod_laplacian3d.f90  -c
${FF} ${OPT_FORT} mod_laplacian_block3d.f90  -c
${FF} ${OPT_FORT} laplacian3d.f90 -c
${LINKER} ${OPT_LINKER} laplacian3d.o mod_laplacian_block3d.o mod_laplacian3d.f90 mod_miscelanos.o  mod_timer.o mod_inparam.o mod_cmdline.o cc_laplacian_block3d.o mod_profile.o mod_types.o timer.o -lrt -o laplacian3d_${COMPILER}_${ARCH}
${FF} ${OPT_ASM_FORT} mod_laplacian3d.f90 -c -o mod_laplacian3d.S.${COMPILER}_${ARCH}
${FF} ${OPT_ASM_FORT} mod_laplacian_block3d.f90 -c -o mod_laplacian_block3d.S.${COMPILER}_${ARCH}
${CC} ${OPT_ASM_CC_OPT} cc_laplacian_block3d.c -c -o cc_laplacian_block3d.S.${COMPILER}_${ARCH}
set +x

