There's a merge conflict only on the current side of the outer conflict.

  $ cat > input.ml <<EOF
  > <<<<<<<<<<<<<< Merlin:HEAD
  > <<<<<<< HEAD
  > A
  > =======
  > |||||||||||||| Compiler:last-imported
  > B
  > ==============
  > C
  > >>>>>>>>>>>>>> Compiler:HEAD
  > D
  > <<<<<<<<<<<<<< Merlin:HEAD
  > E
  > >>>>>>> merlin-branch
  > |||||||||||||| Compiler:last-imported
  > F
  > ==============
  > G
  > >>>>>>>>>>>>>> Compiler:HEAD
  > EOF

  $ combine-merge-conflicts input.ml -o output.ml
  $ cat output.ml
  <<<<<<<<<<<<<< Merlin:HEAD
  <<<<<<< HEAD
  A
  =======
  D
  E
  >>>>>>> merlin-branch
  |||||||||||||| Compiler:last-imported
  B
  D
  F
  ==============
  C
  D
  G
  >>>>>>>>>>>>>> Compiler:HEAD
