#!/bin/sh

# usage: sh manifest_library.sh <library root>
#
# Writes unix.manifest, which names the files of the unix library in
# <library root>, for a MANIFEST_FILES_ROOT that is the root directory.

set -eu

directory=${1#/}/unix
for file in unix.cmi unix.cmx unix.cmxa unix.a; do
  echo "file $file $directory/$file"
done > unix.manifest
