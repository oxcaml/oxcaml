#!/bin/sh

set -e

ocamlsrcdir="$1"
ocamlopt="${ocamlsrcdir}/ocamlopt.opt -nostdlib -I ${ocamlsrcdir}/stdlib"
ocamlopt="${ocamlopt} -extension laws"

${ocamlopt} -i roundtrip.ml > roundtrip.mli
${ocamlopt} -c roundtrip.mli
${ocamlopt} -c roundtrip.ml
