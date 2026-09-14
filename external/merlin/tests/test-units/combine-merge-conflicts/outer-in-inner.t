If an outer conflict occurs within an inner conflict, we interchange them such
that the inner is in the outer.

  $ cat > input.ml <<EOF
  > <<<<<<< current
  > A
  > ||||||| merge-base
  > <<<<<<<<<<<<<< Merlin:current
  > B
  > |||||||||||||| Compiler:last-imported
  > C
  > ==============
  > D
  > >>>>>>>>>>>>>> Compiler:incoming
  > =======
  > E
  > >>>>>>> incoming
  > EOF

  $ combine-merge-conflicts input.ml -o output.ml
  $ cat output.ml
  <<<<<<<<<<<<<< Merlin:current
  <<<<<<< current
  A
  ||||||| merge-base
  B
  =======
  E
  >>>>>>> incoming
  |||||||||||||| Compiler:last-imported
  <<<<<<< current
  A
  ||||||| merge-base
  C
  =======
  E
  >>>>>>> incoming
  ==============
  <<<<<<< current
  A
  ||||||| merge-base
  D
  =======
  E
  >>>>>>> incoming
  >>>>>>>>>>>>>> Compiler:incoming
