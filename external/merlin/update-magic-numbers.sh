#!/bin/bash

set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"

# This script updates the magic numbers in src/ocaml/utils/config.ml and
# src/ocaml/typing/magic_numbers.ml based on the magic numbers in
# upstream/ocaml_flambda/configure

magic_file=src/ocaml/typing/magic_numbers.ml
config_file=src/ocaml/utils/config.ml
version_test_file=tests/test-dirs/version.t

function usage () {
  cat <<USAGE
Usage: $0 MAGIC_NUMBER VERSION

Update the magic numbers in $config_file and $magic_file.

MAGIC_NUMBER defines the magic numbers. It corresponds to MAGIC_NUMBER__VERSION in build-aux/ocaml_version.m4 in the compiler.
VERSION is the OCaml version that this version of Merlin corresponds to.
USAGE
}

if [[ $# = 2 ]]; then
  magic_number_version="$1"
  version="$2"
else
  usage >&2
  exit 1
fi

# Check that git repo is clean
if [ -n "$(git status --porcelain)" ]; then
  echo "Git workspace is unclean.\nCommit changes before continuing to ensure changes don't get overwritten" >&2
  exit 1
fi

# Construct a magic number based on a character used to characterize a file type and a
# number to denote version. See DEFINE_MAGIC_NUMBER in build-aux/ocaml_version.v4 in the
# compiler
function make_magic_number() {
  prefix="${2:-Caml1999}"
  echo "${prefix}${1}${magic_number_version}"
}

# Update src/ocaml/typing/magic_numbers.ml
if grep -q "$version" "$magic_file"; then
  echo "$magic_file already contains magic number for $version; skipping"
else
  echo "Updating magic numbers in $magic_file"
  cmi_magic_number=$(make_magic_number I)
  line_to_insert="| \"$cmi_magic_number\" -> Some \"$version\""
  # The below sed looks for a line like:
  #    | "Caml1999I551" -> Some "5.2.0minus-1"
  # followed by a line like:
  #    | _ -> None
  # and inserts $line_to_insert between them.
  # The :a;N;$!ba; is some sed magic that makes it read in the entire file before
  # substituting, which is necessary to match mutli-line patterns.
  sed -i -E ':a;N;$!ba;s/(\| "[a-zA-Z0-9]+" -> Some "\S*")\n(\s*)(\| _ -> None)/\1\n\2'"$line_to_insert"'\n\2\3/' "$magic_file"
fi

# Update the magic numbers in src/ocaml/utils/config.ml
echo "Updating magic numbers in $config_file and $version_test_file"
function replace_magic_number () {
  name="$1"
  value=$(make_magic_number "${@:2}")
  sed -i 's/let '"$name"' = "[^"]*"/let '"$name"' = "'"$value"'"/' "$config_file"
  sed -i 's/"'"$name"'": "[^"]*"/"'"$name"'": "'"$value"'"/' "$version_test_file"
}
replace_magic_number cmi_magic_number I
replace_magic_number ast_impl_magic_number M
replace_magic_number ast_intf_magic_number N
replace_magic_number cmt_magic_number T
replace_magic_number cms_magic_number S
replace_magic_number index_magic_number I Merl2023

# After updating magic numbers, check that all strings in $config_file are one of
# the above magic numbers (or some other known string). This helps ensure that there were
# not any new magic numbers added that this script does not know about.
# This isn't important to do in $version_test_file since tests would catch the mistake.
okay_strings="[A-Z][a-z]{3}[0-9]{4}[A-Z]$magic_number_version|.mli|[a-z_]+_magic_number"
regex='"(?!'"$okay_strings"')[^"]*"'
if grep -q -P "$regex" "$config_file"; then
  echo "Warning: There may be new magic numbers in $config_file that were not updated:" >&2
  grep -P "$regex" "$config_file" >&2
fi
