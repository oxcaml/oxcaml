#!/bin/bash

# Usage: ./oxcaml/tests/backend/llvmize/run_tests.sh [branch]

# Run this from the root directory of oxcaml!!
ROOT_DIR=$(pwd)
BASE_DIR=$ROOT_DIR/_build/llvm_test # Is this safe to do?

LLVM_SRC_DIR_NAME=llvm-project
LLVM_BUILD_DIR_NAME=llvm_build
LLVM_INSTALL_DIR_NAME=llvm_install_ocaml

LLVM_SRC_DIR=$BASE_DIR/$LLVM_SRC_DIR_NAME
LLVM_BUILD_DIR=$BASE_DIR/$LLVM_BUILD_DIR_NAME
LLVM_INSTALL_DIR=$BASE_DIR/$LLVM_INSTALL_DIR_NAME

if [ ! -d $BASE_DIR ]; then
  mkdir $BASE_DIR
fi

if [ ! -d $LLVM_SRC_DIR ]; then
  echo "Cloning LLVM"
  cd $BASE_DIR
  git clone git@github.com:ocaml-flambda/llvm-project.git # Change if needed
fi

if [ ! -d $LLVM_BUILD_DIR ]; then
  echo "Configuring LLVM"
  mkdir $LLVM_BUILD_DIR
  cd $LLVM_BUILD_DIR
  cmake3 \
    -G Ninja \
    -DLLVM_ENABLE_PROJECTS='clang;lldb;lld;mlir' \
    -DLLVM_ENABLE_RUNTIMES='libcxx;libcxxabi;libunwind;compiler-rt' \
    -DLLVM_RUNTIME_TARGETS='x86_64-unknown-linux-gnu' \
    -DLLVM_HOST_TRIPLE='x86_64-unknown-linux-gnu' \
    -DLLDB_EXPORT_ALL_SYMBOLS=1 \
    -DCMAKE_BUILD_TYPE=Debug \
    -DCMAKE_C_COMPILER=clang \
    -DCMAKE_CXX_COMPILER=clang++ \
    -DLLDB_INCLUDE_TESTS=OFF \
    -DLLVM_BUILD_LLVM_DYLIB=ON \
    -DCLANG_DEFAULT_PIE_ON_LINUX=off \
    -DLLVM_ENABLE_LTO=Off \
    -DLLVM_USE_LINKER=lld \
    -DENABLE_LINKER_BUILD_ID=ON \
    -DCMAKE_INSTALL_PREFIX=../$LLVM_INSTALL_DIR_NAME \
    ../$LLVM_SRC_DIR_NAME/llvm
fi

if [ $# -eq 1  ]; then
  echo "Checking out branch '$1'"
  cd $LLVM_SRC_DIR
  git checkout $1
fi

echo "Building LLVM"
cd $LLVM_BUILD_DIR
cmake3 --build . --target install

echo "Running Oxcaml llvmize tests"
cd $ROOT_DIR
LLVM_PATH=$LLVM_INSTALL_DIR/bin/clang make runtest-llvm
