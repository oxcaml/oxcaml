An inner conflict starts outside the outer conflict, but finishes within it.
  $ cat > input.ml <<EOF
  > <<<<<<< current
  > A
  > =======
  > <<<<<<<<<<<<<< Merlin:current
  > B
  > >>>>>>> incoming
  > |||||||||||||| Compiler:last-imported
  > C
  > >>>>>>> incoming
  > ==============
  > D
  > >>>>>>> incoming
  > >>>>>>>>>>>>>> Compiler:incoming
  > EOF

  $ combine-merge-conflicts input.ml -o output.ml
  $ cat output.ml
  <<<<<<<<<<<<<< Merlin:current
  <<<<<<< current
  A
  =======
  B
  >>>>>>> incoming
  |||||||||||||| Compiler:last-imported
  <<<<<<< current
  A
  =======
  C
  >>>>>>> incoming
  ==============
  <<<<<<< current
  A
  =======
  D
  >>>>>>> incoming
  >>>>>>>>>>>>>> Compiler:incoming
