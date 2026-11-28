#!/bin/sh

# Helpers of the tests of the link-time guard of the codegen variant: the
# symbol caml_runtime_codegen_variant_is_<axes> that a runtime defines for
# the frame-pointer, stack-check and AddressSanitizer settings it was built
# for, and that every object file of generated code refers to.
#
#   sh link_guard.sh runtime <library tree> <copy>
#     Copies the runtime archive of a library tree: libasmrun.a in the tree
#     of the base variant, libasmrun_<axes>.a in the others.
#
#   sh link_guard.sh refusal <file> <log>
#     Says why the link that wrote <log> failed, in words that depend
#     neither on the linker nor on the variants. <file> is an object file
#     or an archive of generated code that took part in the link: what the
#     linker must report as undefined is the guard that <file> refers to.
#
#   sh link_guard.sh refuse <file> <command> <argument>...
#     Runs a command that must fail, a link or a program that loads a
#     plugin, and says why as above.
#
#   sh link_guard.sh runtime-suffix <library root> <compiler>...
#     For each variant tree whose runtime archive has a name of its own,
#     libasmrun<suffix>.a, links a program compiled for the base variant
#     with that tree on the search path and -runtime-variant <suffix>, the
#     way that archive can be given for a debug or instrumented runtime of
#     the base variant. Every such link must be refused.
#
#   sh link_guard.sh generic-functions <plugin>
#     Says whether a plugin defines generic functions (caml_apply<n>,
#     caml_curry<n>, ...).
#
#   sh link_guard.sh variant-error <log>
#     Says whether the compiler's output in <log> is the error for a file
#     compiled for another codegen variant, whose text names the variants.
#
#   sh link_guard.sh shared-runtime <library tree> <copy>
#     Like "runtime", for the runtime as a shared library.
#
#   sh link_guard.sh refuse-start <program> <argument>...
#     Runs a program that the runtime must refuse to start, because its code
#     was compiled for another variant or because the runtime of another
#     variant is loaded with it, and says whether it did.
#
#   sh link_guard.sh stopped <program> <argument>...
#     Like "refuse-start", for a program that the runtime must stop for
#     either reason.
#
#   sh link_guard.sh exports <plugin>...
#     Says whether each plugin exports a guard to the programs that load it,
#     or that it cannot tell.
#
#   sh link_guard.sh sectioned <object file>
#     Says whether an object file has sections of their own for its
#     functions (-function-sections).
#
#   sh link_guard.sh refers <assembly file>
#     Says whether an assembly file that the compiler kept refers to a guard.
#
#   sh link_guard.sh needs <shared object> <object file>
#     Says whether a shared object still needs, from outside it when it is
#     loaded, the guard that an object file linked into it refers to.
#
#   sh link_guard.sh self-test <object file>
#     Checks "refusal" itself, against the diagnostics of the linkers of the
#     supported systems for the guard of <object file>, whatever system the
#     test runs on.
#
#   sh link_guard.sh self-test-two-guards <archive>
#     Checks that "refusal" has no verdict for an archive whose members
#     refer to two guards, when the linker names one of them.

# The guards that a file needs or defines, one per line, without the
# underscore that some systems put in front of C symbols. A weak reference
# (w or v) is not a need: the startup code of a runtime has one to the
# guard of every variant, and an object file made with
# -output-complete-obj contains it.
guards_of() {
  nm "$1" 2>/dev/null | grep -v ' [wv] ' \
    | grep -o 'caml_runtime_codegen_variant_is_[a-z_]*' | sort -u
}

# Whether the log reports the symbol as undefined, alone or with the
# underscore that some systems put in front of it, and not a longer symbol
# that contains it: in the words of GNU ld, of lld and the dynamic linker
# of glibc, of the macOS linker, of the dynamic linker of musl and of that
# of macOS, each with the symbol where that linker puts it. A word of
# theirs elsewhere on the line, in the name of a file say, is not a report.
symbol_end='([^A-Za-z0-9_]|$)'
undefined_in() {
  grep -E -e "undefined reference to ._?$1$symbol_end" \
    -e "undefined symbol: _?$1$symbol_end" \
    -e "\"_?$1\", referenced from" \
    -e "(^|[^A-Za-z0-9_])_?$1: symbol not found" \
    -e "ymbol not found( in flat namespace .|: )_?$1$symbol_end" \
    "$2" > /dev/null
}

# Whether the log reports two definitions of the symbol that every runtime
# defines next to its guard: in the words of GNU ld, lld and the macOS
# linker.
unique=caml_runtime_codegen_variant_must_be_unique
two_runtimes_in() {
  duplicate='(multiple definition of .|duplicate symbol:? .?)'
  grep -E "${duplicate}_?$unique$symbol_end" "$1" > /dev/null
}

# The symbols that a shared object defines for what loads it or is loaded
# after it, and those that it needs from there: the dynamic symbols where
# the format has such a table, and the external symbols elsewhere. A symbol
# that is defined has an address, in the first column. Both fail when the
# file cannot be read.
exported_symbols() {
  table=$(nm -D "$1" 2>/dev/null) || table=$(nm -g "$1" 2>/dev/null) \
    || return 1
  echo "$table" | grep -E '^[0-9a-fA-F]+ ' || true
}
needed_symbols() {
  nm -D -u "$1" 2>/dev/null || nm -u "$1" 2>/dev/null
}

# In what follows, a command that reads a pipe reads all of it: one that
# stops at what it looks for (grep -q, head) leaves the writer with a
# closed pipe, which is an error message in the output of the test where
# the signal for it is ignored.

refusal() {
  guard=$(guards_of "$1")
  count=$(printf '%s\n' "$guard" | grep -c .)
  if [ "$count" -ne 1 ]; then
    echo "$1 does not refer to exactly one guard, but to $count"
    return
  fi
  if undefined_in "$guard" "$2"; then
    echo "refused: nothing defines the guard of the code"
  elif two_runtimes_in "$2"; then
    echo "refused: two runtimes"
  else
    echo "failed for another reason:"
    cat "$2"
  fi
}

mode=$1
shift
case $mode in
  runtime)
    runtime=$(ls "$1"/libasmrun.a "$1"/libasmrun_*.a 2>/dev/null \
                | grep -v '/libasmrun_pic')
    cp "$runtime" "$2"
    ;;
  refusal)
    refusal "$1" "$2"
    ;;
  refuse)
    file=$1
    shift
    if "$@" > refuse.log 2>&1; then
      echo "not refused"
      cat refuse.log
    else
      refusal "$file" refuse.log
    fi
    ;;
  runtime-suffix)
    root=$1
    shift
    unset OCAMLPARAM
    "$@" -c -o default_unit.cmx link_guard_unit.ml || exit 2
    tried=0
    for runtime in "$root"/variants/*/libasmrun_*.a; do
      case $runtime in
        */libasmrun_pic*) continue ;;
        *.a) ;;
      esac
      [ -f "$runtime" ] || continue
      suffix=$(basename "$runtime" .a)
      suffix=${suffix#libasmrun}
      tried=$((tried + 1))
      if "$@" -I "$root" -I "$(dirname "$runtime")" \
           -runtime-variant "$suffix" default_unit.cmx -o suffix.exe \
           > suffix.log 2>&1; then
        echo "not refused: -runtime-variant $suffix"
      else
        verdict=$(refusal default_unit.o suffix.log)
        case $verdict in
          "refused: nothing defines the guard of the code") ;;
          *) echo "-runtime-variant $suffix: $verdict" ;;
        esac
      fi
    done
    if [ "$tried" -gt 0 ]; then
      echo "the runtime of every other variant was tried"
    else
      echo "no runtime of another variant to try"
    fi
    ;;
  generic-functions)
    if symbols=$(nm "$1" 2>/dev/null); then
      if echo "$symbols" \
           | grep -E ' [TtWw] _?caml_(apply|curry|send|tuplify)' > /dev/null
      then
        echo "$1 defines generic functions"
      else
        echo "$1 defines no generic function"
      fi
    else
      echo "$1 cannot be inspected"
    fi
    ;;
  variant-error)
    if tr '\n' ' ' < "$1" | sed -e 's/  */ /g' \
         | grep 'was compiled with codegen configuration' > /dev/null; then
      echo "refused: compiled for another codegen variant"
    else
      echo "no such error:"
      cat "$1"
    fi
    ;;
  shared-runtime)
    cp "$1"/libasmrun_shared*.so "$2"
    ;;
  refuse-start)
    # The runtime aborts: the shell that waits for the program, and says
    # so, must be one whose output goes to the log.
    # It need not leave a core file for that.
    ( ulimit -c 0 2>/dev/null; "$@"; echo "exit status: $?" ) > start.log 2>&1
    if grep -q '^exit status: 0$' start.log; then
      echo "not refused"
      cat start.log
    elif grep -q 'started by a runtime of another codegen variant' start.log
    then
      echo "refused: the runtime that starts is not the one of the code"
    elif grep -q 'runtime of another codegen variant is loaded' start.log
    then
      echo "refused: two runtimes are loaded"
    else
      echo "failed for another reason:"
      cat start.log
    fi
    ;;
  stopped)
    ( ulimit -c 0 2>/dev/null; "$@"; echo "exit status: $?" ) > start.log 2>&1
    if grep -q '^exit status: 0$' start.log; then
      echo "not refused"
      cat start.log
    elif grep -q -e 'started by a runtime of another codegen variant' \
           -e 'runtime of another codegen variant is loaded' start.log
    then
      echo "refused: the runtime stopped the program"
    else
      echo "failed for another reason:"
      cat start.log
    fi
    ;;
  exports)
    for plugin; do
      if symbols=$(exported_symbols "$plugin"); then
        if echo "$symbols" \
             | grep 'caml_runtime_codegen_variant_is_' > /dev/null; then
          echo "$plugin exports a guard"
        else
          echo "$plugin exports no guard"
        fi
      else
        echo "$plugin cannot be inspected"
      fi
    done
    ;;
  refers)
    if grep -q 'caml_runtime_codegen_variant_is_' "$1"; then
      echo "$1 refers to a guard"
    else
      echo "$1 refers to no guard"
    fi
    ;;
  needs)
    guard=$(guards_of "$2")
    if [ "$(printf '%s\n' "$guard" | grep -c .)" -ne 1 ]; then
      echo "$2 does not refer to exactly one guard"
    elif symbols=$(needed_symbols "$1"); then
      if echo "$symbols" | grep -E "(^| )_?$guard\$" > /dev/null; then
        echo "$1 needs the guard of $2"
      else
        echo "$1 does not need the guard of $2"
      fi
    else
      echo "$1 cannot be inspected"
    fi
    ;;
  sectioned)
    if sections=$(objdump -h "$1" 2>/dev/null); then
      if echo "$sections" | grep '\.text\.caml\.' > /dev/null; then
        echo "$1 has sections of their own for its functions"
      else
        echo "$1 has one section for its functions"
      fi
    else
      echo "$1 cannot be inspected"
    fi
    ;;
  self-test)
    guard=$(guards_of "$1")
    verdict() {
      printf '%s\n' "$2" > self-test.log
      echo "$1: $(refusal "$3" self-test.log | sed -n 1p)"
    }
    verdict "GNU ld" \
      "ld: x.o:(.data+0x0): undefined reference to \`$guard'" "$1"
    verdict "lld" "ld.lld: error: undefined symbol: $guard" "$1"
    verdict "ld64" "  \"_$guard\", referenced from:" "$1"
    verdict "ld64.lld" "ld64.lld: error: undefined symbol: _$guard" "$1"
    verdict "the dynamic linker of musl" \
      "Error relocating ./p.so: $guard: symbol not found" "$1"
    verdict "the dynamic linker of macOS" \
      "dlopen(p.so, 0x0A): symbol not found in flat namespace '_$guard'" "$1"
    verdict "a longer symbol" \
      "ld: x.o: undefined reference to \`${guard}_and_more'" "$1"
    verdict "a symbol that ends with it" \
      "ld: x.o: undefined reference to \`not_$guard'" "$1"
    verdict "two definitions of the guard" \
      "ld: y.o: multiple definition of \`$guard'; x.o: first defined here" "$1"
    verdict "two runtimes, GNU ld" \
      "ld: b.a(g.o): multiple definition of \`$unique'; a.a(g.o): first" "$1"
    verdict "two runtimes, lld" "ld.lld: error: duplicate symbol: $unique" "$1"
    verdict "two runtimes, ld64" "duplicate symbol '_$unique' in:" "$1"
    verdict "two definitions of something else" \
      "ld: y.o: multiple definition of \`helper'; x.o: first defined here" "$1"
    verdict "two runtimes, a file named after a word of the linker" \
      "ld: undefined/g.o: multiple definition of \`$guard'; a.a(g.o): first
ld: undefined/g.o: multiple definition of \`$unique'; a.a(g.o): first" "$1"
    ;;
  self-test-two-guards)
    guard=$(guards_of "$1" | sed -n 1p)
    printf '%s\n' "ld: undefined reference to \`$guard'" > self-test.log
    refusal "$1" self-test.log | sed -e "s|$1|the archive|"
    ;;
  *)
    echo "link_guard.sh: unknown mode $mode"
    exit 2
    ;;
esac
