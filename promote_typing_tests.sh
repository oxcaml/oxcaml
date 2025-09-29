#!/bin/bash
# Script to build the compiler and promote layout/mode/jkind test outputs
# This is useful when making changes that affect type inference for modes and layouts

set -e

echo "Building compiler..."
make boot-compiler

echo ""
echo "Promoting test outputs for layout/mode/jkind tests..."

# List of test directories that are affected by layout/mode/jkind changes
TEST_DIRS=(
  "typing-jkind-bounds"
  "typing-layouts"
  "typing-layouts-arrays"
  "typing-layouts-bits16"
  "typing-layouts-bits32"
  "typing-layouts-bits64"
  "typing-layouts-bits8"
  "typing-layouts-block-indices"
  "typing-layouts-caml-modify"
  "typing-layouts-err-msg"
  "typing-layouts-float32"
  "typing-layouts-float64"
  "typing-layouts-gadt-sort-var"
  "typing-layouts-missing-cmi"
  "typing-layouts-or-null"
  "typing-layouts-products"
  "typing-layouts-return"
  "typing-layouts-untagged-immediate"
  "typing-layouts-vec128"
  "typing-layouts-void"
  "typing-layouts-word"
  "typing-modal-kinds"
  "typing-modes"
)

for dir in "${TEST_DIRS[@]}"; do
  echo "Promoting tests in $dir..."
  make promote-one-no-rebuild DIR="$dir"
done

echo ""
echo "Done! All test outputs have been promoted."
echo "Review the changes with: git diff testsuite/tests/"