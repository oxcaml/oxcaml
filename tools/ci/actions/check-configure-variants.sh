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

# Check what configure decides about codegen variants in situations that no
# build of the compiler meets.
#
# - A C compiler that cannot link an AddressSanitizer executable, as a
#   Clang installed without the AddressSanitizer runtime library of
#   compiler-rt cannot: it compiles with -fsanitize=address and fails at
#   link time. A plain configure must then leave the AddressSanitizer
#   variants out of the list it builds, and asking for AddressSanitizer
#   must be an error.
# - A variant list without frame pointers: the configured C flags must be
#   those of a build without variants. They keep the frame pointer only
#   where a variant with frame pointers is built, and there they keep it
#   whatever flags the user adds, to the C flags or to the preprocessor
#   flags that follow them on the command line: this compiles a function
#   with them and looks at the code.
# - A name in --with-codegen-variants that is a file name pattern: it is not
#   a variant name, whatever files it matches.
#
#   check-configure-variants.sh [CC]
#
# CC (default: clang) must be a Clang for x86_64 Linux that can link
# AddressSanitizer executables. Run it in a source tree whose configure
# script has been generated; it configures in temporary directories and
# changes nothing in the tree.

set -euo pipefail
srcdir=$(cd "$(dirname "$0")/../../.." && pwd)
cc=$(command -v "${1:-clang}") || { echo "$0: no ${1:-clang}" >&2; exit 2; }
# configure runs in other directories.
case $cc in /*) ;; *) cc=$PWD/$cc ;; esac
[ -x "$srcdir/configure" ] ||
  { echo "$0: $srcdir/configure is missing: run autoconf" >&2; exit 2; }
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# configure uses the value of CC as a command line, which a path with a
# space in it is not: the compilers go by plain names in a directory of
# PATH. The second is the same compiler without the library: its links with
# -fsanitize=address fail, and nothing else does.
mkdir "$work/bin"
export CHECK_CONFIGURE_VARIANTS_CC=$cc
cat > "$work/bin/cc-with-asan-runtime" <<'END'
#!/bin/sh
exec "$CHECK_CONFIGURE_VARIANTS_CC" "$@"
END
cat > "$work/bin/cc-without-asan-runtime" <<'END'
#!/bin/sh
link=true
for arg; do
  case $arg in -c | -E | -S | -M | -MM) link=false ;; esac
done
if $link; then
  for arg; do
    case $arg in
      -fsanitize=address)
        echo "ld: cannot find libclang_rt.asan.a" >&2
        exit 1 ;;
    esac
  done
fi
exec "$CHECK_CONFIGURE_VARIANTS_CC" "$@"
END
chmod +x "$work/bin/cc-with-asan-runtime" "$work/bin/cc-without-asan-runtime"
PATH=$work/bin:$PATH

# run_configure NAME CC [OPTION]: in $work/NAME; the exit code goes to the
# file status and the output to the file log.
run_configure() {
  local name=$1 compiler=$2 status=0
  shift 2
  mkdir -p "$work/$name"
  cd "$work/$name"
  "$srcdir/configure" CC="$compiler" "$@" > log 2>&1 || status=$?
  echo "$status" > status
}
(run_configure control cc-with-asan-runtime) &
(run_configure plain cc-without-asan-runtime) &
(run_configure variant cc-without-asan-runtime \
  --with-codegen-variants=asan-fp) &
(run_configure default cc-without-asan-runtime \
  --enable-address-sanitizer) &
(run_configure none cc-with-asan-runtime --disable-codegen-variants) &
(run_configure nofp cc-with-asan-runtime --with-codegen-variants=poll,sc) &
(run_configure omit cc-with-asan-runtime --with-codegen-variants=fp-sc \
  COMPILER_BYTECODE_CFLAGS=-fomit-frame-pointer \
  COMPILER_NATIVE_CFLAGS=-fomit-frame-pointer \
  COMPILER_BYTECODE_CPPFLAGS=-fomit-frame-pointer \
  COMPILER_NATIVE_CPPFLAGS=-fomit-frame-pointer) &
# A directory that the pattern matches, where configure runs.
mkdir -p "$work/pattern/poll"
(run_configure pattern cc-with-asan-runtime \
  '--with-codegen-variants=p[oa]ll') &
wait

failed=false
# fail NAME MESSAGE
fail() {
  echo "error: $2" >&2
  echo "The end of the output of that configure:" >&2
  tail -n 15 "$work/$1/log" | sed 's/^/  /' >&2
  failed=true
}
status() { cat "$work/$1/status"; }
# setting NAME VARIABLE: its value in the Makefile.config of the run
setting() { sed -n "s/^$2=//p" "$work/$1/Makefile.config"; }
variants() { setting "$1" CODEGEN_VARIANTS; }
# frame_pointer NAME VARIABLE...: whether a function that calls another,
# compiled with the C flags that the VARIABLEs hold in the run, in that
# order, sets up a frame: prints kept or dropped, or failed if the flags do
# not compile it. The assembly goes to a file and is read once the compiler
# is done, in AT&T syntax whatever the flags ask for.
printf 'void callee(void);\nvoid caller(void) { callee(); callee(); }\n' \
  > "$work/caller.c"
frame_pointer() {
  local name=$1 flags='' variable assembly
  shift
  for variable; do flags="$flags $(setting "$name" "$variable")"; done
  assembly=$work/$name/$(IFS=+; echo "$*").s
  # shellcheck disable=SC2086
  if ! "$cc" $flags -masm=att -S -o "$assembly" "$work/caller.c" \
    > "$assembly.log" 2>&1; then
    echo failed
  elif grep -q 'movq[[:space:]]*%rsp, *%rbp' "$assembly"; then
    echo kept
  else
    echo dropped
  fi
}
without='with a compiler that cannot link AddressSanitizer executables'
cannot_link='cannot link an Address Sanitizer executable'

# Without this, the checks below could pass where they test nothing.
if [ "$(status control)" != 0 ]; then
  fail control "configure CC=$cc failed"
else
  case " $(variants control) " in
    *" asan-fp "*) echo "CC=$cc: builds the variants $(variants control)" ;;
    *) fail control "configure CC=$cc does not select the asan-fp variant" ;;
  esac
fi

# What the control run selected, the AddressSanitizer variants aside.
expected=
for variant in $(variants control); do
  case $variant in
    asan-*) ;;
    *) expected="$expected${expected:+ }$variant" ;;
  esac
done
if [ "$(status plain)" != 0 ]; then
  fail plain "a plain configure failed $without"
else
  if [ "$(variants plain)" = "$expected" ]; then
    echo "No AddressSanitizer runtime library: builds the variants" \
      "$(variants plain)"
  else
    fail plain "a plain configure selected the variants \
[$(variants plain)] $without, not [$expected]"
  fi
  grep -q "not building an Address Sanitizer codegen variant: .*$cannot_link" \
    "$work/plain/log" ||
    fail plain "a plain configure does not say why it builds no \
AddressSanitizer variant"
fi

for run in variant:--with-codegen-variants=asan-fp \
  default:--enable-address-sanitizer; do
  name=${run%%:*}
  option=${run#*:}
  if [ "$(status "$name")" = 0 ]; then
    fail "$name" "configure $option succeeded $without"
  elif ! grep -q "error: .*$cannot_link" "$work/$name/log"; then
    fail "$name" "configure $option failed $without, but for another reason"
  else
    echo "No AddressSanitizer runtime library: $option is an error"
  fi
done

# The C flags that ocamlopt gives to C files, and that build systems read
# from its configuration.
if [ "$(status none)" != 0 ]; then
  fail none "configure --disable-codegen-variants failed"
elif [ "$(status nofp)" != 0 ]; then
  fail nofp "configure --with-codegen-variants=poll,sc failed"
elif [ "$(status control)" = 0 ]; then
  flags=$(setting none OCAMLOPT_CFLAGS)
  if [ "$(setting nofp OCAMLOPT_CFLAGS)" != "$flags" ]; then
    fail nofp "the variants poll and sc change the configured C flags from \
[$flags] to [$(setting nofp OCAMLOPT_CFLAGS)]"
  else
    echo "Variants without frame pointers: C flags as with no variant"
  fi
  case " $flags " in
    *" -fno-omit-frame-pointer "*)
      fail none "the C flags keep the frame pointer with no variant: \
[$flags]" ;;
  esac
  case $(frame_pointer control OCAMLOPT_CFLAGS OCAMLOPT_CPPFLAGS) in
    kept)
      echo "Variants with frame pointers: C code keeps the frame pointer" ;;
    dropped) fail control "a variant with frame pointers is built and C \
code compiled with the configured flags has no frame pointer: \
[$(setting control OCAMLOPT_CFLAGS)]" ;;
    *) fail control "the configured C flags do not compile a C file: \
[$(setting control OCAMLOPT_CFLAGS)]" ;;
  esac
fi

# The C flags by themselves, and with the preprocessor flags after them, as
# ocamlopt and ocamlc put them on the command line of a C compilation.
if [ "$(status omit)" != 0 ]; then
  fail omit "configure failed with the COMPILER_*_CFLAGS and \
COMPILER_*_CPPFLAGS variables"
else
  for variables in OCAMLOPT_CFLAGS OCAMLC_CFLAGS \
    "OCAMLOPT_CFLAGS OCAMLOPT_CPPFLAGS" "OCAMLC_CFLAGS OCAMLC_CPPFLAGS"; do
    # shellcheck disable=SC2086
    case $(frame_pointer omit $variables) in
      kept)
        echo "Variants with frame pointers, -fomit-frame-pointer added by" \
          "the user: C code compiled with $variables keeps the frame" \
          "pointer" ;;
      dropped) fail omit "a variant with frame pointers is built and the \
user's -fomit-frame-pointer removes the frame pointer from C code \
compiled with $variables" ;;
      *) fail omit "the configured flags do not compile a C file: \
$variables" ;;
    esac
  done
fi

if [ "$(status pattern)" = 0 ]; then
  fail pattern "configure --with-codegen-variants='p[oa]ll' took the name \
of a directory for the variant: $(variants pattern)"
elif ! grep -q "error: unknown codegen variant name 'p\[oa\]ll'" \
  "$work/pattern/log"; then
  fail pattern "configure --with-codegen-variants='p[oa]ll' failed, but \
not for the name"
else
  echo "A file name pattern is not a variant name"
fi

if $failed; then exit 1; fi
