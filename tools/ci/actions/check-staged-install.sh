#!/usr/bin/env bash

#**************************************************************************#
#*                                                                        *#
#*                                 OCaml                                  *#
#*                                                                        *#
#*                  Jacob Van Buren, Jane Street, New York                *#
#*                                                                        *#
#*   Copyright 2026 Jane Street Group LLC                                 *#
#*                                                                        *#
#*   All rights reserved.  This file is distributed under the terms of    *#
#*   the GNU Lesser General Public License version 2.1, with the          *#
#*   special exception on linking described in the file LICENSE.          *#
#*                                                                        *#
#**************************************************************************#

# Check that a staged install, `make install DESTDIR=...`, leaves the
# prefix itself alone. `make install` assembles the installation in _install
# of the build tree, then copies it to $(DESTDIR)$(prefix): outside the
# build tree, it must write and delete under $(DESTDIR)$(prefix) only.
# Installing over a prefix that holds an earlier installation removes what
# must not outlive it (the library trees of codegen variants that are no
# longer built, the cached generic functions): none of that may reach the
# prefix itself when the install is staged.
#
#   check-staged-install.sh
#
# Run it at the root of a configured tree. It runs `make install` and then
# `make install_for_opam`, which only copies _install, for a prefix and a
# staging directory of its own, both temporary, and compares the prefix
# after each of the two with what it was before it. The prefix has to be
# outside the build tree: with the development prefix, $PWD/_install, the
# directory that `make install` assembles is the prefix, and it is rebuilt
# whatever DESTDIR is. For that prefix the script checks one thing:
# installing _install into itself copies nothing.
# As every `make install` does, this one builds what is out of date and
# assembles _install anew, without what other targets have added to it
# (`make install_for_test` adds js_of_ocaml and wasm_of_ocaml when the
# tests run under JSIR): run `make install_for_test` after this script, not
# before.

set -euo pipefail
[ -f Makefile.config ] ||
  { echo "$0: run it at the root of a configured tree" >&2; exit 2; }
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
prefix=$work/prefix
# A directory name is not shell syntax.
stage="$work/the builder's stage"
staged=$stage$prefix
cache=lib/ocaml/cached-generic-functions.a
stale_tree=lib/ocaml/variants/no-such-variant

failed=false
fail() { echo "error: $1" >&2; failed=true; }

# The prefix itself: its entries, each with its type, the permissions of a
# file or a directory, the target of a link and the contents of a file.
prefix_state() {
  (cd "$prefix" && find . | LC_ALL=C sort | while IFS= read -r entry; do
    mode=$(ls -ld "$entry"); mode=${mode%% *}
    if [ -L "$entry" ]; then echo "link $entry -> $(readlink "$entry")"
    elif [ -d "$entry" ]; then echo "directory $mode $entry"
    elif [ -f "$entry" ]; then echo "file $mode $entry $(cksum < "$entry")"
    else echo "other $entry"
    fi
  done)
}

# Run a target of make for the prefix and the staging directory: the prefix
# itself must be afterwards what it was before that target. For make, a `$`
# in a value on its command line starts a reference, and `$$` stands for the
# character.
staged_make() {
  make -s "$1" prefix="${prefix//\$/\$\$}" DESTDIR="${stage//\$/\$\$}"
  now=$(prefix_state)
  [ "$now" = "$earlier" ] || {
    fail "make $1 DESTDIR=... changed the prefix itself:"
    diff <(echo "$earlier") <(echo "$now") | sed -n '1,10p' >&2 || true
  }
  earlier=$now
}

# What an earlier installation left in the prefix: a file and a link that
# an installation has, and the library tree of a variant that this one does
# not build.
mkdir -p "$prefix/$stale_tree" "$prefix/bin" "$stage"
echo earlier > "$prefix/$cache"
ln -s earlier "$prefix/bin/ocamlopt"
earlier=$(prefix_state)
staged_make install

[ -x "$staged/bin/ocamlopt.opt" ] ||
  fail "the staging directory has no compiler in $staged/bin"
for variant in $(sed -n 's/^CODEGEN_VARIANTS=//p' Makefile.config); do
  [ -f "$staged/lib/ocaml/variants/$variant/stdlib.cmxa" ] ||
    fail "the staging directory has no library tree for $variant"
done

# The same removals must happen inside a staging directory that holds an
# earlier installation. Its cached generic functions give way to those of
# this installation, if it has any, and so does a file that differs from
# this installation's in its contents only: same size, same time.
mkdir -p "$staged/$stale_tree"
echo earlier > "$staged/$cache"
lookalike=lib/ocaml/stdlib.cmxa
head -c "$(wc -c < "_install/$lookalike")" /dev/zero > "$staged/$lookalike"
touch -r "_install/$lookalike" "$staged/$lookalike"
staged_make install_for_opam
cmp -s "_install/$lookalike" "$staged/$lookalike" ||
  fail "a staged install over an earlier one kept a $lookalike of the \
same size and time as this installation's, with other contents"
if [ -e "_install/$cache" ]; then
  cmp -s "_install/$cache" "$staged/$cache" ||
    fail "a staged install over an earlier one did not replace its $cache"
else
  [ ! -e "$staged/$cache" ] ||
    fail "a staged install over an earlier one kept its $cache"
fi
[ ! -e "$staged/$stale_tree" ] ||
  fail "a staged install over an earlier one kept a library tree that \
this one does not build"

# Installing _install into itself, which is what the development prefix
# asks of `make install`, copies nothing: rsync, which would read every
# file twice to compare the tree with itself, does not run.
mkdir "$work/bin"
printf '#!/bin/sh\nexit 1\n' > "$work/bin/rsync"
chmod +x "$work/bin/rsync"
PATH="$work/bin:$PATH" make -s install_for_opam \
    prefix="${PWD//\$/\$\$}/_install" DESTDIR= ||
  fail "installing _install into itself runs the copy"

if $failed; then exit 1; fi
echo "A staged install leaves the prefix itself alone"
