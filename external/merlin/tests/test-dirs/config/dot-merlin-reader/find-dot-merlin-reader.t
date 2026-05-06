  $ cat > .merlin <<EOF
  > UNIT_NAME Dot_merlin_was_successfully_read
  > EOF


In real-world scenarios, ocamlmerlin and dot-merlin-reader are usually installed in the
same directory (for example, via an opam install). In this situation, if dot-merlin-reader
isn't on the PATH, we can still read the .merlin file.
  $ mkdir merlin-bin
  $ cp $(which dot-merlin-reader) merlin-bin
  $ cp $(which ocamlmerlin) merlin-bin
  $ cp $(which ocamlmerlin-server) merlin-bin
  $ cp $(which ocaml-index) merlin-bin

  $ PATH="" merlin-bin/ocamlmerlin single dump-configuration -filename test.ml \
  > | jq .value.merlin.unit_name -r
  Dot_merlin_was_successfully_read

If ocamlmerlin is a symlink, it looks in the original install directory.

  $ mkdir merlin-bin-symlink
  $ ln -s $(realpath merlin-bin/ocamlmerlin) merlin-bin-symlink/ocamlmerlin

  $ PATH="" merlin-bin-symlink/ocamlmerlin single dump-configuration -filename test.ml \
  > | jq .value.merlin.unit_name -r
  Dot_merlin_was_successfully_read

The dot-merlin-reader in the same directory is prioritized over the one on the PATH.
  $ mkdir bad-dot-merlin-reader-bin
  $ cat > bad-dot-merlin-reader-bin/dot-merlin-reader <<EOF
  > #!/bin/sh
  > echo "this is a bad dot-merlin-reader" >&2
  > exit 1
  > EOF

  $ PATH="bad-dot-merlin-reader-bin" \
  > merlin-bin/ocamlmerlin single dump-configuration -filename test.ml \
  > | jq .value.merlin.unit_name -r
  Dot_merlin_was_successfully_read

But we can fall back to the one on the PATH if a dot-merlin-reader doesn't exist in the
same directory.
  $ cp -r merlin-bin merlin-bin-no-reader
  $ rm merlin-bin-no-reader/dot-merlin-reader

  $ PATH="" \
  > merlin-bin-no-reader/ocamlmerlin single dump-configuration -filename test.ml \
  > | jq .value.merlin.unit_name -r
  null

  $ PATH="merlin-bin" \
  > merlin-bin-no-reader/ocamlmerlin single dump-configuration -filename test.ml \
  > | jq .value.merlin.unit_name -r
  Dot_merlin_was_successfully_read

We can override using the DOT_MERLIN_READER_EXE environment variable.
  $ cp -r merlin-bin merlin-bin-bad-reader
  $ rm merlin-bin-bad-reader/dot-merlin-reader
  $ cp bad-dot-merlin-reader-bin/dot-merlin-reader merlin-bin-bad-reader

  $ PATH="merlin-bin-bad-reader" \
  > merlin-bin-bad-reader/ocamlmerlin single dump-configuration -filename test.ml \
  > | jq .value.merlin.unit_name -r
  null

  $ PATH="merlin-bin-bad-reader" DOT_MERLIN_READER_EXE="merlin-bin/dot-merlin-reader" \
  > merlin-bin-bad-reader/ocamlmerlin single dump-configuration -filename test.ml \
  > | jq .value.merlin.unit_name -r
  Dot_merlin_was_successfully_read
