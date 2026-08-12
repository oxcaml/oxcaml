#!/usr/bin/env bash

set -e -u -o pipefail

THIS_FOLDER=$(cd "$(dirname "$0")"; pwd)
BUILD_FOLDER="${THIS_FOLDER}/../../_build"
REGALLOC_TOOL="${BUILD_FOLDER}/main/tools/regalloc/regalloc.exe"
STRATEGY="default custom"

N=7 # keep in sync with Python code

for strategy in ${STRATEGY}; do
  for n in `seq ${N}`; do
    echo "running '${strategy}' (${n}/${N})..."
    "${REGALLOC_TOOL}" -regalloc ${strategy} -csv-output "${BUILD_FOLDER}" \
      > ${THIS_FOLDER}/compare_strategies_${strategy}_${n}.csv
  done
done