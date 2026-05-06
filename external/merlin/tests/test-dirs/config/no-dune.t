  $ mkdir bin
  $ cp ../../../../install/default/bin/ocamlmerlin bin/ocamlmerlin
  $ cp ../../../../install/default/bin/ocamlmerlin-server bin/ocamlmerlin-server

  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat >main.ml <<EOF
  > print_endline "Hello world"
  > EOF

  $ PATH=bin ocamlmerlin single dump-configuration \
  > -filename main.ml <main.ml >output

  $ cat output | jq '.value.merlin.failures'
  [
    "Merlin could not find `dune` in the PATH to get project configuration. If you do not rely on Dune, make sure `.merlin` files are present in the project's sources."
  ]

  $ cat >.merlin <<EOF
  > S .
  > EOF

  $ PATH=bin ocamlmerlin single dump-configuration \
  > -filename main.ml <main.ml >output

  $ cat output | jq '.value.merlin.failures'
  [
    "Merlin could not find `dot-merlin-reader`. Please make sure that `dot-merlin-reader` is installed. `dot-merlin-reader` is expected to be in the same directory as the merlin executable or on the PATH. You may also specify the path to `dot-merlin-reader` via the `DOT_MERLIN_READER_EXE` environment variable."
  ]
