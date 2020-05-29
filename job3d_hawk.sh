#!/bin/bash
# Simple PBS batch script that reserves one exclusive rome node
# The walltime is 60min
#
#PBS -N Cache_Blocking_HAWK
#PBS -l walltime=00:45:00
##PBS -l select=1:node_type=sb:mpiprocs=16
##PBS -l select=1:node_type=hsw:mpiprocs=24:node_type_core=24c
##PBS -l select=1:node_type=skl:mpiprocs=40:node_type_core=40c
#PBS -l select=1:node_type=rome:mpiprocs=128

#############################################
#     Job script to start the benchmark     #
#             Cache Blocking                #
#           on traning cluster              #
#############################################
PROJECT_DIR=${HOME}/myprojects/exercise_4_cache_blocking
ARCH=HAWK
BENCHMARK_ID=3
VERBOSITY=3
CHECK_SOLUTION=1
CLEAR_CACHE=1
SIZE_X=1024
SIZE_Y=1024
SIZE_Z=512
MIN_BLK_SIZE_X=64
MAX_BLK_SIZE_X=64
STP_BLK_SIZE_X=32
MIN_BLK_SIZE_Y=64
MAX_BLK_SIZE_Y=64
STP_BLK_SIZE_Y=32
MIN_BLK_SIZE_Z=128
MAX_BLK_SIZE_Z=128
STP_BLK_SIZE_Z=32

MIN_TIME=3.0
NUM_MIN_THREADS=1
NUM_MAX_THREADS=128
NUM_STEP_THREADS=1

HAS_NUMAS=1
NUM_NUMAS=8
NUMA_CORES=16

hostname

set -x
DATA_DIR=${PROJECT_DIR}/data3d
JOBID=$( echo $PBS_JOBID |sed 's/\..*//')
if [ -z "${JOBID}" ]; then
  JOBID=0
fi
JOB_DATA_DIR=${DATA_DIR}/data_${JOBID}

if [ ! -d "${JOB_DATA_DIR}" ]; then
  mkdir ${JOB_DATA_DIR} -p
  if [ ! -d "${JOB_DATA_DIR}" ]; then
    echo "Error:  check DATA_DIR: " ${JOB_DATA_DIR}
    exit 1
  fi
fi

for NUM_THREADS in $(seq ${NUM_MIN_THREADS} ${NUM_STEP_THREADS} ${NUM_MAX_THREADS})
do
  export OMP_NUM_THREADS=${NUM_THREADS}
  COMPILER=INTEL
  module load intel
  EXEC=laplacian3d_${COMPILER}_${ARCH}
  set -x # Debug Modus ON
  FILEPATH=${JOB_DATA_DIR}/${COMPILER}_benchmark_${BENCHMARK_ID}_threads_${NUM_THREADS}.csv
  #Start benchmark ${BENCHMARK_ID}
  ${PROJECT_DIR}/${EXEC} -verbosity ${VERBOSITY} -jobid ${JOBID} -benchmark_id ${BENCHMARK_ID} -num_threads ${NUM_THREADS} -size_x ${SIZE_X} -size_y ${SIZE_Y} -size_z ${SIZE_Z} -min_blk_size_x ${MIN_BLK_SIZE_X} -max_blk_size_x ${MAX_BLK_SIZE_X} -step_blk_size_x ${STP_BLK_SIZE_X} -min_blk_size_y ${MIN_BLK_SIZE_Y} -max_blk_size_y ${MAX_BLK_SIZE_Y} -step_blk_size_y ${STP_BLK_SIZE_X} -min_blk_size_z ${MIN_BLK_SIZE_Z} -max_blk_size_z ${MAX_BLK_SIZE_Z} -step_blk_size_z ${STP_BLK_SIZE_Z}  -min_time ${MIN_TIME} -filepath ${FILEPATH} -check_solution ${CHECK_SOLUTION} -clear_cache ${CLEAR_CACHE} -has_numas ${HAS_NUMAS} -num_numas ${NUM_NUMAS} -numa_cores ${NUMA_CORES}

  set +x # Debug Modus OFF
done

