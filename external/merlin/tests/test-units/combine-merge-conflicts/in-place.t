Without -o the file is rewritten in place.

  $ cat > input.ml <<EOF
  > <<<<<<<<<<<<<< Merlin:current
  > <<<<<<< current
  > A
  > =======
  > |||||||||||||| Compiler:last-imported
  > B
  > ==============
  > C
  > >>>>>>>>>>>>>> Compiler:incoming
  > D
  > <<<<<<<<<<<<<< Merlin:current
  > E
  > >>>>>>> incoming
  > |||||||||||||| Compiler:last-imported
  > F
  > ==============
  > G
  > >>>>>>>>>>>>>> Compiler:incoming
  > EOF

  $ combine-merge-conflicts input.ml
  $ cat input.ml
  <<<<<<<<<<<<<< Merlin:current
  <<<<<<< current
  A
  =======
  D
  E
  >>>>>>> incoming
  |||||||||||||| Compiler:last-imported
  B
  D
  F
  ==============
  C
  D
  G
  >>>>>>>>>>>>>> Compiler:incoming
