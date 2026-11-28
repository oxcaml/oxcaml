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
# - The --enable-<axis> and --disable-<axis> options of the four axes
#   (frame pointers, stack checks, poll insertion, AddressSanitizer). They
#   are requirements on the list of variants, with or without
#   --with-codegen-variants, and set no default: the configured C flags,
#   linker flags, macros and compiler settings are those of a configure
#   without them, the variant in the library root is base, and configure
#   ends with a notice that says so.
# - Other targets. No machine is needed for what configure decides from the
#   target: it is given the triplet as the one of the build machine, and
#   tests the C compiler of this machine believing that. An arm64 target,
#   Linux or macOS, has the variants poll and sc, and none with frame
#   pointers or AddressSanitizer. A target with another backend has none,
#   and its C runtime keeps its mandatory stack checks. A build without the
#   native compiler has none either, whatever the triplet.
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
# The runs go on at the same time, one per processor at most.
processors=$(getconf _NPROCESSORS_ONLN 2> /dev/null || echo 4)
# start NAME CC [OPTION]: run_configure in the background
start() {
  while [ "$(jobs -rp | wc -l)" -ge "$processors" ]; do sleep 1; done
  (run_configure "$@") &
}
start control cc-with-asan-runtime
start plain cc-without-asan-runtime
start variant cc-without-asan-runtime --with-codegen-variants=asan-fp
start default cc-without-asan-runtime --enable-address-sanitizer
start none cc-with-asan-runtime --disable-codegen-variants
start nofp cc-with-asan-runtime --with-codegen-variants=poll,sc
start omit cc-with-asan-runtime --with-codegen-variants=fp-sc \
  COMPILER_BYTECODE_CFLAGS=-fomit-frame-pointer \
  COMPILER_NATIVE_CFLAGS=-fomit-frame-pointer \
  COMPILER_BYTECODE_CPPFLAGS=-fomit-frame-pointer \
  COMPILER_NATIVE_CPPFLAGS=-fomit-frame-pointer
# A directory that the pattern matches, where configure runs.
mkdir -p "$work/pattern/poll"
start pattern cc-with-asan-runtime '--with-codegen-variants=p[oa]ll'

# The options of the axes. The name of a run is the name of its directory;
# its options are kept for the messages.
declare -A options
# axis_run NAME [OPTION]: with the control's compiler
axis_run() {
  local name=$1
  shift
  options[$name]=$*
  start "$name" cc-with-asan-runtime "$@"
}
axis_run enable-fp --enable-frame-pointers
axis_run enable-sc --enable-stack-checks
axis_run enable-poll --enable-poll-insertion
axis_run enable-asan --enable-address-sanitizer
axis_run enable-fp-poll --enable-frame-pointers --enable-poll-insertion
axis_run enable-poll-sc --enable-poll-insertion --enable-stack-checks
axis_run enable-asan-poll --enable-address-sanitizer --enable-poll-insertion
axis_run enable-fp-poll-sc --enable-frame-pointers --enable-poll-insertion \
  --enable-stack-checks
axis_run disable-fp --disable-frame-pointers
axis_run disable-sc --disable-stack-checks
axis_run disable-poll --disable-poll-insertion
axis_run disable-asan --disable-address-sanitizer
axis_run enable-fp-disable-sc --enable-frame-pointers --disable-stack-checks
axis_run list-with-required --enable-frame-pointers \
  --with-codegen-variants=fp,sc
axis_run list-without-disabled --disable-frame-pointers \
  --with-codegen-variants=base,poll,poll-sc,sc
axis_run list-without-required --enable-frame-pointers \
  --with-codegen-variants=fp-sc
axis_run list-with-axes-apart --enable-poll-insertion --enable-stack-checks \
  --with-codegen-variants=poll,sc
axis_run list-empty-required --enable-stack-checks --with-codegen-variants=
axis_run list-with-disabled --disable-frame-pointers \
  --with-codegen-variants=poll,fp-sc
axis_run none-required --enable-frame-pointers --disable-codegen-variants
axis_run none-disabled --disable-stack-checks --disable-codegen-variants
axis_run asan-sc --enable-address-sanitizer --enable-stack-checks
axis_run asan-no-fp --enable-address-sanitizer --disable-frame-pointers

# Other targets, with the control's compiler too.
riscv_linux=riscv64-unknown-linux-gnu
# target_run NAME TRIPLET [OPTION]
target_run() {
  local name=$1 triplet=$2
  shift 2
  options[$name]="for $triplet${*:+ with $*}"
  start "$name" cc-with-asan-runtime "--build=$triplet" "--host=$triplet" "$@"
}
for system in linux macos; do
  case $system in
    linux) triplet=aarch64-unknown-linux-gnu ;;
    *) triplet=aarch64-apple-darwin ;;
  esac
  target_run "arm64-$system" "$triplet"
  target_run "arm64-$system-list" "$triplet" \
    --with-codegen-variants=base,poll,poll-sc,sc
  target_run "arm64-$system-enable" "$triplet" --enable-poll-insertion \
    --enable-stack-checks
  target_run "arm64-$system-disable" "$triplet" --disable-stack-checks
  target_run "arm64-$system-fp" "$triplet" --enable-frame-pointers
  target_run "arm64-$system-asan" "$triplet" --enable-address-sanitizer
  target_run "arm64-$system-list-fp" "$triplet" --with-codegen-variants=fp-sc
  target_run "arm64-$system-list-asan" "$triplet" \
    --with-codegen-variants=asan-fp
done
target_run riscv "$riscv_linux"
target_run riscv-sc "$riscv_linux" --enable-stack-checks
target_run riscv-no-sc "$riscv_linux" --disable-stack-checks
target_run riscv-poll "$riscv_linux" --enable-poll-insertion
target_run riscv-list "$riscv_linux" --with-codegen-variants=sc
axis_run bytecode --disable-native-compiler
axis_run bytecode-sc --disable-native-compiler --enable-stack-checks
axis_run bytecode-list --disable-native-compiler --with-codegen-variants=sc
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

# The options of the axes, on the target of the control run. Nothing below
# means anything if that run failed.
if [ "$(status control)" != 0 ]; then exit 1; fi

# succeeded NAME: whether the run did, reporting it if not
succeeded() {
  [ "$(status "$1")" = 0 ] ||
    { fail "$1" "configure ${options[$1]} failed"; return 1; }
}
# builds NAME VARIANT...: the run succeeded and builds exactly these
# variants besides the one of the library root
builds() {
  local name=$1
  shift
  succeeded "$name" || return 0
  if [ "$(variants "$name")" = "$*" ]; then
    echo "configure ${options[$name]}: builds" \
      "${*:-no variant besides the one of the library root}"
  else
    fail "$name" "configure ${options[$name]} selected the variants \
[$(variants "$name")], not [$*]"
  fi
}
# refused NAME PATTERN: the run failed with an error that matches PATTERN
refused() {
  if [ "$(status "$1")" = 0 ]; then
    fail "$1" "configure ${options[$1]} succeeded"
  elif ! grep -q "error: .*$2" "$work/$1/log"; then
    fail "$1" "configure ${options[$1]} failed, but not with an error \
that matches '$2'"
  else
    echo "configure ${options[$1]}: an error"
  fi
}
# sorted VARIANT...: in the order of the list that configure makes
sorted() { printf '%s\n' "$@" | LC_ALL=C sort -u | tr '\n' ' ' | sed 's/ $//'; }
# control_without TOKEN: the variants of the control run that do not have
# the axis
control_without() {
  local variant kept=''
  for variant in $(variants control); do
    case -$variant- in
      *-"$1"-*) ;;
      *) kept="$kept $variant" ;;
    esac
  done
  # shellcheck disable=SC2086
  sorted $kept
}
all=$(variants control)

# An enabled axis: the variant with exactly the enabled axes is built, with
# the automatic list.
# shellcheck disable=SC2046,SC2086
{
  builds enable-fp $all
  builds enable-sc $all
  builds enable-poll $all
  builds enable-asan $all
  builds enable-fp-poll $(sorted $all fp-poll)
  builds enable-poll-sc $(sorted $all poll-sc)
  builds enable-asan-poll $(sorted $all asan-fp-poll)
  builds enable-fp-poll-sc $(sorted $all fp-poll-sc)
  # A disabled axis: no variant with it.
  builds disable-fp $(control_without fp)
  builds disable-sc $(control_without sc)
  builds disable-poll $(control_without poll)
  builds disable-asan $(control_without asan)
  builds enable-fp-disable-sc $(control_without sc)
}
# The same two rules with a list.
builds list-with-required fp sc
builds list-without-disabled poll poll-sc sc
refused list-without-required "'fp' has to be built"
refused list-with-axes-apart "'poll-sc' has to be built"
refused list-empty-required "'sc' has to be built"
refused list-with-disabled "'fp-sc' conflicts with --disable-frame-pointers"
refused none-required "conflicts with --disable-codegen-variants"
builds none-disabled
refused asan-sc "stack checks together with Address Sanitizer"
refused asan-no-fp "conflicts with --disable-frame-pointers"

# No option sets a default. What configure writes for the build is what the
# control run wrote, the list of variants and the record of the options
# aside: the C and linker flags, the settings of the compiler, the macros
# of the runtime's headers.
# configuration NAME: what the run wrote, to compare
configuration() {
  (cd "$work/$1" &&
    grep -v -e '^CODEGEN_VARIANTS=' -e '^CONFIGURE_ARGS=' \
      Makefile.config Makefile.build_config &&
    cat runtime/caml/m.h runtime/caml/s.h utils/config.generated.ml)
}
configuration control > "$work/control.configuration"
for name in enable-fp enable-sc enable-poll enable-asan enable-fp-poll \
  enable-poll-sc enable-asan-poll enable-fp-poll-sc; do
  [ "$(status "$name")" = 0 ] || continue
  configuration "$name" > "$work/$name.configuration"
  if cmp -s "$work/control.configuration" "$work/$name.configuration"; then
    echo "configure ${options[$name]}: sets no default"
  else
    fail "$name" "configure ${options[$name]} changes more of the \
configuration than the list of variants:
$(diff "$work/control.configuration" "$work/$name.configuration" | head -n 20)"
  fi
done
axis_macros='WITH_FRAME_POINTERS|STACK_CHECKS_ENABLED|WITH_ADDRESS_SANITIZER'
if grep -Eq "^#define ($axis_macros)" "$work/control/runtime/caml/m.h"; then
  fail control "m.h defines the macro of an axis"
fi

# The notice: with an option of an axis, and only then.
notice='these options decide which codegen variants are built'
if [ "$(status enable-fp-poll)" = 0 ]; then
  log=$work/enable-fp-poll/log
  if grep -q -- "--enable-frame-pointers --enable-poll-insertion: $notice" \
       "$log" &&
     grep -q "builds the variants: base $(variants enable-fp-poll)\$" \
       "$log" &&
     grep -q "for 'fp-poll', OCAMLPARAM=frame-pointers=1,poll-insertion=1,_" \
       "$log"; then
    echo "configure ${options[enable-fp-poll]}: says that it sets no" \
      "default, what it builds and how to set one"
  else
    fail enable-fp-poll "configure ${options[enable-fp-poll]} does not \
end with the notice about defaults"
  fi
fi
if [ "$(status disable-sc)" = 0 ] &&
   ! grep -q -- "--disable-stack-checks: $notice" "$work/disable-sc/log"; then
  fail disable-sc "configure --disable-stack-checks does not end with the \
notice about defaults"
fi
if grep -q "$notice" "$work/control/log"; then
  fail control "a configure without an option of an axis prints the \
notice about defaults"
fi

# arm64: stack checks and poll insertion only.
unsupported_fp='frame pointers not supported on this platform'
unsupported_asan='Address Sanitizer requires Clang on x86_64 Linux'
for system in linux macos; do
  builds "arm64-$system" poll sc
  builds "arm64-$system-list" poll poll-sc sc
  builds "arm64-$system-enable" poll poll-sc sc
  builds "arm64-$system-disable" poll
  refused "arm64-$system-fp" "$unsupported_fp"
  refused "arm64-$system-asan" "$unsupported_asan"
  refused "arm64-$system-list-fp" "'fp-sc' cannot be built: $unsupported_fp"
  refused "arm64-$system-list-asan" \
    "'asan-fp' cannot be built: $unsupported_asan"
  if [ "$(status "arm64-$system")" = 0 ] &&
     [ "$(setting "arm64-$system" ARCH)" != arm64 ]; then
    fail "arm64-$system" "configure ${options[arm64-$system]} selected the \
backend [$(setting "arm64-$system" ARCH)]: this run tested nothing"
  fi
done

# Another backend: no variants, and the stack checks of the C runtime.
# stack_checks_in_runtime NAME: whether m.h defines the macro
stack_checks_in_runtime() {
  grep -q '^#define STACK_CHECKS_ENABLED' "$work/$1/runtime/caml/m.h"
}
no_variants='codegen variants'
builds riscv
builds riscv-sc
builds riscv-no-sc
refused riscv-poll "--enable-poll-insertion: $no_variants cannot be built"
refused riscv-list "--with-codegen-variants: $no_variants are not supported"
for name in riscv riscv-sc riscv-no-sc; do
  [ "$(status "$name")" = 0 ] || continue
  if stack_checks_in_runtime "$name"; then
    echo "configure ${options[$name]}: the C runtime has stack checks"
  else
    fail "$name" "configure ${options[$name]} leaves the stack checks out \
of the C runtime"
  fi
done
if stack_checks_in_runtime control; then
  fail control "the C runtime of the library root has stack checks"
fi

# No native compiler: no backend, whatever the triplet.
builds bytecode
refused bytecode-sc "--enable-stack-checks: $no_variants cannot be built"
refused bytecode-list "--with-codegen-variants: $no_variants are not supported"

if $failed; then exit 1; fi
