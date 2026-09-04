#!/bin/sh

#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                     Nick Barnes, Jane Street Europe                    *
#*                                                                        *
#*   Copyright 2026 Jane Street Group LLC                                 *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# Emits the dune rules that build the runtime: one rule per object file,
# archive and executable, with the configured commands baked in.  Invoked
# by the rule in duneconf/dune and pulled into runtime/dune by
# (dynamic_include ../duneconf/runtime_rules.inc), so editing this file
# or runtime_build_flags.sh regenerates the rules.  Source lists, flags
# and commands mirror the runtime rules of Makefile.upstream;
# configure-dependent values come from runtime_build_flags.sh (generated
# by configure).  Runs in the duneconf directory; the emitted rules are
# interpreted in the runtime directory, so the paths they contain are
# relative to it.

set -eu

. ./runtime_build_flags.sh

# Unquoted expansions of the flag variables are intentional throughout:
# they hold whitespace-separated flag lists.

# Mirrors runtime_CPPFLAGS, ocamlrund_CPPFLAGS and ocamlruni_CPPFLAGS in
# Makefile.upstream.
RUNTIME_CPPFLAGS='-DCAMLDLLIMPORT= -DIN_CAML_RUNTIME'
DEBUG_CPPFLAGS='-DDEBUG'
INSTR_CPPFLAGS='-DCAML_INSTR'

# Source lists: mirror runtime_COMMON_C_SOURCES,
# runtime_BYTECODE_ONLY_C_SOURCES and runtime_NATIVE_ONLY_C_SOURCES in
# Makefile.upstream.
COMMON_C_SOURCES="addrmap afl alloc array backtrace bigarray blake2 callback
  codefrag compare custom debugger domain dynlink extern fail fiber dynamic
  finalise floats gc_ctrl gc_stats globroots hash intern ints io lexing
  lf_skiplist main major_gc md5 memory memprof meta minor_gc misc obj
  parsing platform printexc prng roots runtime_events shared_heap signals
  simd cpu float32 skiplist startup_aux str sync sys
  $TSAN_NATIVE_RUNTIME_C_SOURCES $UNIX_OR_WIN32 weak"

BYTECODE_ONLY_C_SOURCES="backtrace_byt fail_byt fix_code interp startup_byt
  zstd"

NATIVE_ONLY_C_SOURCES="backtrace_nat clambda_checks dynlink_nat fail_nat
  frame_descriptors startup_nat signals_nat"

BYTECODE_C_SOURCES="$COMMON_C_SOURCES $BYTECODE_ONLY_C_SOURCES"
NATIVE_C_SOURCES="$COMMON_C_SOURCES $NATIVE_ONLY_C_SOURCES"

# Mirrors the per-file target-specific flags in Makefile.upstream.
per_file_cflags () {
  case "$1" in
  # The major GC performs better with this flag on Intel processors;
  # see Makefile.upstream.
  major_gc) printf '%s' "$INTEL_JCC_BUG_CFLAGS";;
  # Partial inlining on (at least) caml_string_compare seems to produce
  # worse code.
  str) printf '%s' "$NO_PARTIAL_INLINING_CFLAGS";;
  *) ;;
  esac
}

# The flags for compiling objects with no variant suffix (prims.o, sak.o).
PLAIN_FLAGS="$OC_CFLAGS $CFLAGS $OC_CPPFLAGS $RUNTIME_CPPFLAGS $CPPFLAGS"

# compile_rules <suffix> <variant cflags> <variant cppflags> <basenames...>:
# one rule per <basename>, compiling <basename>.c to <basename>.<suffix>.o.
compile_rules () {
  suffix="$1"; variant_cflags="$2"; variant_cppflags="$3"; shift 3
  for base in "$@"; do
    obj="$base.$suffix.o"
    cat <<EOF
(rule
 (targets $obj)
 (deps $base.c build_config.h caml/domain_state.tbl caml/jumptbl.h
   caml/opnames.h caml/version.h (glob_files *.h) (glob_files caml/*.h))
 (action
  (run $CC -c $OC_CFLAGS $variant_cflags $(per_file_cflags "$base") $CFLAGS
    $OC_CPPFLAGS $RUNTIME_CPPFLAGS $variant_cppflags $CPPFLAGS
    -o $obj $base.c)))
EOF
  done
}

# objects <suffix> <basenames...>: the object names emitted by compile_rules
objects () {
  suffix="$1"; shift
  for base in "$@"; do printf '%s.%s.o ' "$base" "$suffix"; done
}

# The assembly objects of libasmrun{,d,i,_pic}.a.  RUNTIME_ASM_OBJECTS
# holds the unsuffixed object names (e.g. "amd64.o").
asm_object () { # asm_object <variant> <unsuffixed object>
  case "$1" in
  n) printf '%s' "$2";;
  nd) printf '%s' "${2%.o}.d.o";;
  ni) printf '%s' "${2%.o}.i.o";;
  npic) printf '%s' "${2%.o}_libasmrunpic.o";;
  esac
}

asm_objects () {
  for obj in $RUNTIME_ASM_OBJECTS; do
    printf '%s ' "$(asm_object "$1" "$obj")"
  done
}

# asm_rules <variant: n|nd|ni|npic>
asm_rules () {
  case "$1" in
  n) extra='';;
  nd) extra="$DEBUG_CPPFLAGS";;
  ni) extra="$INSTR_CPPFLAGS";;
  npic) extra="$SHAREDLIB_CFLAGS";;
  esac
  for obj in $RUNTIME_ASM_OBJECTS; do
    out="$(asm_object "$1" "$obj")"
    src="${obj%.o}.S"
    cat <<EOF
(rule
 (targets $out)
 (deps $src caml/domain_state.tbl (glob_files *.h) (glob_files caml/*.h))
 (action
  (run $ASPP $OC_CPPFLAGS $RUNTIME_CPPFLAGS $OC_NATIVE_CPPFLAGS $extra
    -o $out $src)))
EOF
  done
}

# archive_rule <output> <objects...>: mirrors MKLIB in Makefile.config.
# The rm keeps ar from updating a leftover archive in place.
archive_rule () {
  out="$1"; shift
  cat <<EOF
(rule
 (targets $out)
 (mode fallback)
 (deps $*)
 (action
  (progn
   (run rm -f $out)
   (run $AR rc $out $*))))
EOF
}

# exe_rule <output> <link flags> <trailing libs> <inputs...>:
# mirrors the ocamlrun link commands in Makefile.upstream.
exe_rule () {
  out="$1"; linkflags="$2"; libs="$3"; shift 3
  cat <<EOF
(rule
 (targets $out)
 (mode fallback)
 (deps $*)
 (action (run $MKEXE $linkflags -o $out $* $libs)))
EOF
}

# dll_rule <output> <trailing libs> <objects...>
dll_rule () {
  out="$1"; libs="$2"; shift 2
  cat <<EOF
(rule
 (targets $out)
 (mode fallback)
 (deps $*)
 (action (run $MKDLL -o $out $* $libs)))
EOF
}

echo "; Generated by gen_rules.sh: do not edit."

# sak, the build-system Swiss Army Knife, and the files made with it
if [ "$WITH_ADDRESS_SANITIZER" = true ]; then
  sak_link_flags='-fsanitize=address -fsanitize-recover=address'
else
  sak_link_flags=''
fi
cat <<EOF
(rule
 (targets sak.o)
 (deps sak.c caml/domain_state.tbl (glob_files caml/*.h))
 (action (run $CC -c $PLAIN_FLAGS -o sak.o sak.c)))
(rule
 (targets sak)
 (deps sak.o)
 (action (run $CC -o sak sak.o $sak_link_flags)))
(rule
 (targets build_config.h)
 (mode fallback)
 (deps sak)
 (action
  (with-stdout-to build_config.h
   (progn
    (echo "/* This file is generated by gen_runtime_rules.sh */\n")
    (echo "#define OCAML_STDLIB_DIR ")
    (run ./sak $ENCODE_C_LITERAL "$LIBDIR")
    (echo "\n#define HOST \\"$HOST\\"\n")))))
(rule
 (targets ld.conf)
 (mode fallback)
 (action (with-stdout-to ld.conf (echo "$STUBLIBDIR\n$LIBDIR\n"))))
(rule
 (targets prims.o)
 (deps prims.c caml/domain_state.tbl (glob_files caml/*.h))
 (action (run $CC -c $PLAIN_FLAGS -o prims.o prims.c)))
EOF

# The bytecode runtime
compile_rules b '' '' $BYTECODE_C_SOURCES
compile_rules bd '' "$DEBUG_CPPFLAGS" $BYTECODE_C_SOURCES instrtrace
compile_rules bi '' "$INSTR_CPPFLAGS" $BYTECODE_C_SOURCES
compile_rules bpic "$SHAREDLIB_CFLAGS" '' $BYTECODE_C_SOURCES

archive_rule libcamlrun.a $(objects b $BYTECODE_C_SOURCES)
archive_rule libcamlrund.a $(objects bd $BYTECODE_C_SOURCES instrtrace)
archive_rule libcamlruni.a $(objects bi $BYTECODE_C_SOURCES)
archive_rule libcamlrun_pic.a $(objects bpic $BYTECODE_C_SOURCES)

exe_rule ocamlrun '' "$BYTECCLIBS" prims.o libcamlrun.a
exe_rule ocamlrund "$MKEXEDEBUGFLAG" "$BYTECCLIBS" prims.o libcamlrund.a
exe_rule ocamlruni '' "$INSTRUMENTED_RUNTIME_LIBS $BYTECCLIBS" \
  prims.o libcamlruni.a

dll_rule libcamlrun_shared.so "$BYTECCLIBS" \
  $(objects bpic $BYTECODE_C_SOURCES)

# The native runtime
compile_rules n "$OC_NATIVE_CFLAGS" "$OC_NATIVE_CPPFLAGS" $NATIVE_C_SOURCES
compile_rules nd "$OC_NATIVE_CFLAGS" "$OC_NATIVE_CPPFLAGS $DEBUG_CPPFLAGS" \
  $NATIVE_C_SOURCES
compile_rules ni "$OC_NATIVE_CFLAGS" "$OC_NATIVE_CPPFLAGS $INSTR_CPPFLAGS" \
  $NATIVE_C_SOURCES
compile_rules npic "$OC_NATIVE_CFLAGS $SHAREDLIB_CFLAGS" \
  "$OC_NATIVE_CPPFLAGS" $NATIVE_C_SOURCES

asm_rules n
asm_rules nd
asm_rules ni
asm_rules npic

archive_rule libasmrun.a $(objects n $NATIVE_C_SOURCES) $(asm_objects n)
archive_rule libasmrund.a $(objects nd $NATIVE_C_SOURCES) $(asm_objects nd)
archive_rule libasmruni.a $(objects ni $NATIVE_C_SOURCES) $(asm_objects ni)
archive_rule libasmrun_pic.a \
  $(objects npic $NATIVE_C_SOURCES) $(asm_objects npic)

dll_rule libasmrun_shared.so "$NATIVECCLIBS" \
  $(objects npic $NATIVE_C_SOURCES) $(asm_objects npic)
