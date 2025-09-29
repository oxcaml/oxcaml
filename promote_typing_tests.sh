#!/bin/bash
# Script to build the compiler and promote all typing test outputs
# This is useful when making changes that affect type inference

set -e

echo "Building compiler..."
make boot-compiler

echo ""
echo "Promoting test outputs for all typing tests..."

# All typing-related test directories
# Ordered by likelihood of being affected by layout/mode/jkind changes
TEST_DIRS=(
  # HIGHEST PRIORITY: Layout tests with products/modes (unboxed records/tuples with local_)
  "typing-layouts-products"
  "typing-layouts-or-null"
  "typing-jkind-bounds"

  # HIGH PRIORITY: Mode and modality tests (directly affected by axis ordering)
  "typing-modes"
  "typing-modal-kinds"
  "typing-local"

  # MEDIUM-HIGH PRIORITY: Other layout tests that may involve mode crossing
  "typing-layouts"
  "typing-unique"
  "typing-zero-alloc"
  "typing-layouts-arrays"

  # MEDIUM PRIORITY: Specific layout types
  "typing-layouts-float32"
  "typing-layouts-float64"
  "typing-layouts-void"
  "typing-layouts-bits8"
  "typing-layouts-bits16"
  "typing-layouts-bits32"
  "typing-layouts-bits64"
  "typing-layouts-vec128"
  "typing-layouts-word"
  "typing-simd"
  "typing-small-numbers"

  # LOWER PRIORITY: Layout infrastructure tests
  "typing-layouts-block-indices"
  "typing-layouts-caml-modify"
  "typing-layouts-err-msg"
  "typing-layouts-gadt-sort-var"
  "typing-layouts-missing-cmi"
  "typing-layouts-return"
  "typing-layouts-untagged-immediate"
  "typing-kind"
  "typing-immediate"
  "typing-unboxed"
  "typing-unboxed-types"

  # LOW PRIORITY: Module and signature tests (less likely affected by axis ordering)
  "typing-modules"
  "typing-modules-bugs"
  "typing-signatures"
  "typing-sigsubst"
  "typing-fstclassmod"
  "typing-recmod"

  # LOW PRIORITY: Type system features
  "typing-gadts"
  "typing-poly"
  "typing-poly-bugs"
  "typing-polyvariants-bugs"
  "typing-polyvariants-bugs-2"
  "typing-objects"
  "typing-objects-bugs"
  "typing-private"
  "typing-private-bugs"
  "typing-rectypes-bugs"
  "typing-typeparam"

  # LOWEST PRIORITY: Other typing tests
  "typing-extension-constructor"
  "typing-extensions"
  "typing-external"
  "typing-labeled-tuples"
  "typing-labels"
  "typing-recordarg"
  "typing-warnings"
  "typing-core-bugs"
  "typing-misc"
  "typing-misc-bugs"
  "typing-deprecated"
  "typing-implicit-source-positions"
  "typing-implicit_unpack"
  "typing-missing-cmi"
  "typing-missing-cmi-2"
  "typing-missing-cmi-3"
  "typing-missing-cmi-indirections"
  "typing-multifile"
  "typing-ocamlc-i"
  "typing-safe-linking"
  "typing-shadowing-of-pervasives-submodules"
  "typing-short-paths"
)

for dir in "${TEST_DIRS[@]}"; do
  echo "Promoting tests in $dir..."
  make promote-one-no-rebuild DIR="$dir"
done

echo ""
echo "Done! All test outputs have been promoted."
echo "Review the changes with: git diff testsuite/tests/"