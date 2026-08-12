**************************
When building without Dune
**************************

  $ $OCAMLC -c -bin-annot anothermod.mli 
  $ $OCAMLC -c -bin-annot anothermod.ml 


  $ cat >.merlin << EOF
  > EOF

Jump from to another module `module A = Anothe|rmod`:
  $ $MERLIN single locate -look-for ml -position 1:21 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/anothermod.ml",
    "pos": {
      "line": 1,
      "col": 0
    }
  }

Jump from to another module signature `module A = Anothe|rmod`:
  $ $MERLIN single locate -look-for mli -position 1:21 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/anothermod.mli",
    "pos": {
      "line": 1,
      "col": 0
    }
  }

Jump to an element of an aliased module `A.|f`:
  $ $MERLIN single locate -look-for ml -position 5:7 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/anothermod.ml",
    "pos": {
      "line": 3,
      "col": 4
    }
  }

Jump to the declaration of an element of an alisaed module `A.|f`:
  $ $MERLIN single locate -look-for mli -position 5:7 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/anothermod.mli",
    "pos": {
      "line": 3,
      "col": 4
    }
  }

Jump to an aliased module `A|.f`:
  $ $MERLIN single locate -look-for ml -position 5:2 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/anothermod.ml",
    "pos": {
      "line": 1,
      "col": 0
    }
  }

Jump to the declaration of an aliased module `A|.f`.
The alias is traversed.
  $ $MERLIN single locate -look-for mli -position 5:2 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/anothermod.mli",
    "pos": {
      "line": 1,
      "col": 0
    }
  }

# Jane Street: we deleted the second half of the file that uses dune in tests. We can't run dune in tests yet.
