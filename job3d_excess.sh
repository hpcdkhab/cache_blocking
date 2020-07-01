#!/bin/bash
# Simple PBS batch script that reserves one exclusive rome node
# The walltime is 60min
#
#PBS -N Cache_Blocking_HASWELL
#PBS -l walltime=00:45:00
#PBS -l nodes=node03:ppn=24
#PBS -m bea
#PBS -o /nas_home/hpcdkhab/myprojects/cache_blocking/messages/${PBS_JOBID}.out
#PBS -e /nas_home/hpcdkhab/myprojects/cache_blocking/messages/${PBS_JOBID}.err
##PBS -l select=1:node_type=sb:mpiprocs=16
##PBS -l select=1:node_type=hsw:mpiprocs=24:node_type_core=24c
##PBS -l select=1:node_type=skl:mpiprocs=40:node_type_core=40c
#PBS -l select=1:node_type=rome:mpiprocs=128

#############################################
#     Job script to start the benchmark     #
#             Cache Blocking                #
#           on traning cluster              #
#############################################
PROJECT_DIR=${HOME}/myprojects/cache_blocking
ARCH=HASWELL
BENCHMARK_ID=2
VERBOSITY=3
CHECK_SOLUTION=0
CLEAR_CACHE=0
SIZE_X=1024
SIZE_Y=1024
SIZE_Z=512
#SIZE_X=128
#SIZE_Y=128
#SIZE_Z=512
MIN_BLK_SIZE_X=64
MAX_BLK_SIZE_X=64
STP_BLK_SIZE_X=32
MIN_BLK_SIZE_Y=64
MAX_BLK_SIZE_Y=64
STP_BLK_SIZE_Y=32
MIN_BLK_SIZE_Z=128
MAX_BLK_SIZE_Z=128
STP_BLK_SIZE_Z=32
NUM_REPETITIONS=1
MIN_TIME=0.0001
NUM_MIN_THREADS=24
NUM_MAX_THREADS=24
NUM_STEP_THREADS=1

HAS_NUMAS=1
NUM_NUMAS=2
NUMA_CORES=12

hostname

DATA_DIR=${PROJECT_DIR}/data
JOBID=$( echo $PBS_JOBID |sed 's/\..*//')
if [ -z "${JOBID}" ]; then
  JOBID=0
fi
JOB_DATA_DIR=${DATA_DIR}/data_${JOBID}
PROFILE_PATH=${JOB_DATA_DIR}/profile.dat
HAS_PROFILE=1

if [ ! -d "${JOB_DATA_DIR}" ]; then
  mkdir ${JOB_DATA_DIR} -p
  if [ ! -d "${JOB_DATA_DIR}" ]; then
    echo "Error:  check DATA_DIR: " ${JOB_DATA_DIR}
    exit 1
  fi
fi

COMPILER=INTEL
module load compiler/intel/parallel_studio_xe_2019_update5
for core in $(seq 0 23 1)
do
  sudo use_cpufreq -c 4 -p ${core}
done
for NUM_THREADS in $(seq ${NUM_MIN_THREADS} ${NUM_STEP_THREADS} ${NUM_MAX_THREADS})
do
  for freq_stp in {14..0..1}
  do
    for core in `seq 0 23`; do sudo use_cpufreq -f ${freq_stp} -p ${core}; done
    echo "---------------------------"
    echo "NUM_THREADS:"${NUM_THREADS}
    echo "freq_stp:"${freq_stp}
    use_cpufreq -p 0 -c 5
    export OMP_NUM_THREADS=${NUM_THREADS}
    EXEC=laplacian3d_${COMPILER}_${ARCH}
    FILEPATH=${JOB_DATA_DIR}/${COMPILER}_benchmark_${BENCHMARK_ID}_threads_${NUM_THREADS}_freq_${freq_stp}.csv
    #Start benchmark ${BENCHMARK_ID}
    echo ${PROJECT_DIR}/${EXEC} -verbosity ${VERBOSITY} -jobid ${JOBID} -benchmark_id ${BENCHMARK_ID} -num_threads ${NUM_THREADS} -size_x ${SIZE_X} -size_y ${SIZE_Y} -size_z ${SIZE_Z} -min_blk_size_x ${MIN_BLK_SIZE_X} -max_blk_size_x ${MAX_BLK_SIZE_X} -step_blk_size_x ${STP_BLK_SIZE_X} -min_blk_size_y ${MIN_BLK_SIZE_Y} -max_blk_size_y ${MAX_BLK_SIZE_Y} -step_blk_size_y ${STP_BLK_SIZE_X} -min_blk_size_z ${MIN_BLK_SIZE_Z} -max_blk_size_z ${MAX_BLK_SIZE_Z} -step_blk_size_z ${STP_BLK_SIZE_Z}  -min_time ${MIN_TIME} -filepath ${FILEPATH} -check_solution ${CHECK_SOLUTION} -clear_cache ${CLEAR_CACHE} -has_numas ${HAS_NUMAS} -num_numas ${NUM_NUMAS} -numa_cores ${NUMA_CORES}  -num_repetitions ${NUM_REPETITIONS} -profile_filepath ${PROFILE_PATH} -has_profile ${HAS_PROFILE}
    ${PROJECT_DIR}/${EXEC} -verbosity ${VERBOSITY} -jobid ${JOBID} -benchmark_id ${BENCHMARK_ID} -num_threads ${NUM_THREADS} -size_x ${SIZE_X} -size_y ${SIZE_Y} -size_z ${SIZE_Z} -min_blk_size_x ${MIN_BLK_SIZE_X} -max_blk_size_x ${MAX_BLK_SIZE_X} -step_blk_size_x ${STP_BLK_SIZE_X} -min_blk_size_y ${MIN_BLK_SIZE_Y} -max_blk_size_y ${MAX_BLK_SIZE_Y} -step_blk_size_y ${STP_BLK_SIZE_X} -min_blk_size_z ${MIN_BLK_SIZE_Z} -max_blk_size_z ${MAX_BLK_SIZE_Z} -step_blk_size_z ${STP_BLK_SIZE_Z}  -min_time ${MIN_TIME} -filepath ${FILEPATH} -check_solution ${CHECK_SOLUTION} -clear_cache ${CLEAR_CACHE} -has_numas ${HAS_NUMAS} -num_numas ${NUM_NUMAS} -numa_cores ${NUMA_CORES}  -num_repetitions ${NUM_REPETITIONS} -profile_filepath ${PROFILE_PATH} -has_profile ${HAS_PROFILE}
    echo "---------------------------"
  done
  sleep 1
done

for core in `seq 0 23`; do echo $core; sudo use_cpufreq -f 0 -p ${core}; sudo use_cpufreq -c 3 -p ${core} ; done
