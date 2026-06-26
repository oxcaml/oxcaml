Test locating a definition in a file where the file has the same name and
contents as another file.

  $ export MERLIN_JANE_CONTEXT=jane
  $ touch jenga.conf

Use the physical (symlink-resolved) path of the test directory throughout.
The compiler resolves symlinks when recording its build directory, and Dune
may run this test inside a symlinked sandbox, so the physical path is the one
we must match against.

  $ ROOT=$(pwd -P)

The compiler records its working directory in the cms files. We use
BUILD_PATH_PREFIX_MAP to record the working directory the same way as Jane
Street dune does.

  $ export BUILD_PATH_PREFIX_MAP="/jenga-root=$ROOT/_build/default"

Create and build a single library. Arguments: library name, capitalized module
prefix, and whether the [foo.ml(i)] source is present in the source tree ("yes")
or only in the build directory ("no", simulating a generated file).

  $ create_and_build_lib () {
  >   name="$1"; prefix="$2"; src_present="$3"
  >   mkdir -p "$name" "_build/default/$name"
  >   echo "let foo = ()" > "_build/default/$name/foo.ml"
  >   echo "val foo : unit" > "_build/default/$name/foo.mli"
  >   echo "module Foo = ${prefix}__Foo" > "_build/default/$name/$name.ml"
  >   cp "_build/default/$name/$name.ml" "$name/$name.ml"
  >   if [ "$src_present" = "yes" ]; then
  >     cp "_build/default/$name/foo.ml" "$name/foo.ml"
  >     cp "_build/default/$name/foo.mli" "$name/foo.mli"
  >   fi
  >   ( cd _build/default &&
  >     $OCAMLC -c -bin-annot-cms -I "$name" \
  >       -o "$name/${prefix}__Foo.cmi" "$name/foo.mli" &&
  >     $OCAMLC -c -bin-annot-cms -I "$name" \
  >       -o "$name/${prefix}__Foo.cmo" "$name/foo.ml" &&
  >     $OCAMLC -c -bin-annot-cms -I "$name" "$name/$name.ml" )
  >   { echo "B $ROOT/_build/default/$name"
  >     echo "S $ROOT/$name"
  >     echo "SOURCE_ROOT $ROOT"
  >   } > "_build/default/$name/.merlin"
  > }

  $ create_and_build_lib lib_nogen1 Lib_nogen1 yes
  $ create_and_build_lib lib_nogen2 Lib_nogen2 yes
  $ create_and_build_lib lib_gen1 Lib_gen1 no
  $ create_and_build_lib lib_gen2 Lib_gen2 no

The client references the [Foo] of each library.

  $ mkdir -p client _build/default/client
  $ cat > _build/default/client/main.ml <<EOF
  > let () = Lib_nogen1.Foo.foo
  > let () = Lib_nogen2.Foo.foo
  > let () = Lib_gen1.Foo.foo
  > let () = Lib_gen2.Foo.foo
  > EOF
  $ cp _build/default/client/main.ml client/main.ml

  $ { echo "B $ROOT/_build/default/lib_nogen1"
  >   echo "B $ROOT/_build/default/lib_nogen2"
  >   echo "B $ROOT/_build/default/lib_gen1"
  >   echo "B $ROOT/_build/default/lib_gen2"
  >   echo "B $ROOT/_build/default/client"
  >   echo "SOURCE_ROOT $ROOT"
  > } > _build/default/client/.merlin

Locate both the definition (ml) and the declaration (mli) of [foo] at the
given position in client/main.ml.

  $ locate () {
  >   line="$1"; col="$2"
  >   echo "definition:"
  >   $MERLIN single locate -look-for ml -position "$line:$col" \
  >     -filename client/main.ml < client/main.ml \
  >     | jq '.value' | sed "s|$ROOT|\$TESTCASE_ROOT|g"
  >   echo "declaration:"
  >   $MERLIN single locate -look-for mli -position "$line:$col" \
  >     -filename client/main.ml < client/main.ml \
  >     | jq '.value' | sed "s|$ROOT|\$TESTCASE_ROOT|g"
  > }

Lib_nogen1.Foo.foo

  $ locate 1 25
  definition:
  {
    "file": "$TESTCASE_ROOT/lib_nogen1/foo.ml",
    "pos": {
      "line": 1,
      "col": 4
    }
  }
  declaration:
  {
    "file": "$TESTCASE_ROOT/lib_nogen1/foo.mli",
    "pos": {
      "line": 1,
      "col": 4
    }
  }

Lib_nogen2.Foo.foo

  $ locate 2 25
  definition:
  {
    "file": "$TESTCASE_ROOT/lib_nogen2/foo.ml",
    "pos": {
      "line": 1,
      "col": 4
    }
  }
  declaration:
  {
    "file": "$TESTCASE_ROOT/lib_nogen2/foo.mli",
    "pos": {
      "line": 1,
      "col": 4
    }
  }

Lib_gen1.Foo.foo

  $ locate 3 23
  definition:
  {
    "file": "$TESTCASE_ROOT/_build/default/lib_gen1/foo.ml",
    "pos": {
      "line": 1,
      "col": 4
    }
  }
  declaration:
  {
    "file": "$TESTCASE_ROOT/_build/default/lib_gen1/foo.mli",
    "pos": {
      "line": 1,
      "col": 4
    }
  }

Lib_gen2.Foo.foo

  $ locate 4 23
  definition:
  {
    "file": "$TESTCASE_ROOT/_build/default/lib_gen2/foo.ml",
    "pos": {
      "line": 1,
      "col": 4
    }
  }
  declaration:
  {
    "file": "$TESTCASE_ROOT/_build/default/lib_gen2/foo.mli",
    "pos": {
      "line": 1,
      "col": 4
    }
  }
