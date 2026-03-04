#!/usr/bin/env bash

set -e -u -o pipefail

THIS_FOLDER=$(cd "$(dirname "$0")"; pwd)
BUILD_FOLDER="${THIS_FOLDER}/../../_build"
REGALLOC_TOOL="${BUILD_FOLDER}/main/tools/regalloc/regalloc.exe"
ALLOCATORS="irc ls"

N=7 # keep in sync with Python code

for allocator in ${ALLOCATORS}; do
  for n in `seq ${N}`; do
    echo "running '${allocator}' (${n}/${N})..."
    "${REGALLOC_TOOL}" -regalloc ${allocator} -csv-output "${BUILD_FOLDER}" \
      > ${THIS_FOLDER}/learn_linscan_choice_${allocator}_${n}.csv
  done
done
