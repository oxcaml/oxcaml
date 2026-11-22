#!/bin/sh

# Helper for variant_variables.ml: uses cc, cflags, mkexe and stdlib the way
# the scripts of tests do.

set -eu

case "$1" in
  native)
    # A stub compiled with cflags, in a program linked against stdlib.
    $cc $cflags -I"$runtime" -c variant_variables_.c \
      -o "variant_variables_.$objext"
    $ocamlrun $ocamlopt_byte -nostdlib -I "$stdlib" \
      "variant_variables_.$objext" variant_variables.ml -o stub.exe
    ./stub.exe
    # A C program compiled and linked with mkexe.
    $mkexe -DSTANDALONE -I"$runtime" -o standalone.exe variant_variables_.c
    ./standalone.exe
    ;;
  own-stdlib)
    if [ "$stdlib" != "$2" ]; then
      echo "stdlib is $stdlib, and the test assigned $2"
      exit 1
    fi
    ;;
  bytecode)
    for flags in "$cflags" "$mkexe"; do
      case "$flags" in
        *CAML_CODEGEN_VARIANT_EXPLICIT*)
          echo "C flags of a codegen variant in a bytecode block: $flags"
          exit 1;;
      esac
    done
    # The native compiler that this script starts compiles for the variant:
    # the link succeeds only against the variant's library tree.
    echo 'let () = ()' > native_unit.ml
    $ocamlrun $ocamlopt_byte -nostdlib -I "$stdlib" native_unit.ml \
      -o native_unit.exe
    ;;
  order)
    # The two programs of the block agree on what their stub saw, which
    # the block has checked.  Under a variant that is not the default one,
    # which has a library tree of its own, it must be the macro: the
    # variant's part of cflags comes after the -U of the test.
    if [ "$stdlib" != "$ocamlsrcdir/stdlib" ]; then
      expected="cflags: FROM_TEST STACK_CHECKS_ENABLED"
      if [ "$(cat flags.output)" != "$expected" ]; then
        echo "the stub of a variant with stack checks saw:"
        cat flags.output
        exit 1
      fi
    fi
    ;;
  driver)
    # The compiler by itself, without the variables of ocamltest, whose
    # part for the variant would do its work for it: given the configured
    # C flags again in -ccopt, as a build that names its C compiler does,
    # ocamlopt neither instruments the stub of a variant without
    # AddressSanitizer nor links the sanitizer's runtime, also where the
    # configured flags ask for both.
    configured=$($ocamlrun $ocamlopt_byte -config-var ocamlopt_cflags)
    $ocamlrun $ocamlopt_byte -nostdlib -I "$stdlib" -ccopt "$configured" \
      variant_variables_.c variant_variables.ml -o driver.exe
    result=$(./driver.exe instrumentation)
    if [ "$result" != "AddressSanitizer: as the variant has it" ]; then
      echo "with the configured flags in -ccopt: $result"
      exit 1
    fi
    ;;
esac
