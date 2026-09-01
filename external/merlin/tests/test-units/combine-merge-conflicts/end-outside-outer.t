An inner conflict beins within the outer conflict, but finishes outside it.
  $ cat > input.ml <<EOF
  > <<<<<<<<<<<<<< Merlin:current
  > <<<<<<< current
  > A
  > =======
  > |||||||||||||| Compiler:last-imported
  > <<<<<<< current
  > B
  > =======
  > ==============
  > <<<<<<< current
  > C
  > =======
  > >>>>>>>>>>>>>> Compiler:incoming
  > D
  > >>>>>>> incoming
  > EOF

  $ combine-merge-conflicts input.ml -o output.ml
  $ cat output.ml
  <<<<<<<<<<<<<< Merlin:current
  <<<<<<< current
  A
  =======
  D
  >>>>>>> incoming
  |||||||||||||| Compiler:last-imported
  <<<<<<< current
  B
  =======
  D
  >>>>>>> incoming
  ==============
  <<<<<<< current
  C
  =======
  D
  >>>>>>> incoming
  >>>>>>>>>>>>>> Compiler:incoming
