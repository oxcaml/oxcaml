Test using the dump-file-stats subcommand

mtime will not be consistent between tests, so don't include it
  $ normalize () {
  >   cat | sed -E "s/mtime=[0-9\.]+/mtime=<mtime>/g"
  > }

Create a small project
  $ mkdir foo
  $ cat > foo/a.ml << EOF
  > let hello = "hello"
  > EOF
  $ cat > foo/b.ml << EOF
  > let world = "world"
  > EOF

Compile the project and create an index file
  $ $OCAMLC -c -bin-annot -bin-annot-occurrences foo/a.ml foo/b.ml
  $ ocaml-index aggregate foo/a.cmt foo/b.cmt -o foo.merlin-index

Dump the file stats from the index
  $ ocaml-index dump-file-stats foo.merlin-index | normalize
  File stats for index "foo.merlin-index":
    "foo/a.ml": { mtime=<mtime>; size=20; source_digest="\147<\155xp\000:M2\170\163\134K`\235\226" }
    "foo/b.ml": { mtime=<mtime>; size=20; source_digest="erv\218\000\233\177\190e\189\199\026q\156\195#" }
