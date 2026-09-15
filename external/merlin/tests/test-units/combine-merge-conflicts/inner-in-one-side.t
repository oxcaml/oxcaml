  $ cat > input.ml <<EOF
  > <<<<<<<<<<<<<< Merlin:current
  > <<<<<<< current
  > A
  > =======
  > B
  > >>>>>>> incoming
  > |||||||||||||| Compiler:last-imported
  > C
  > ==============
  > D
  > >>>>>>>>>>>>>> Compiler:incoming
  > E
  > <<<<<<<<<<<<<< Merlin:current
  > F
  > |||||||||||||| Compiler:last-imported
  > G
  > ==============
  > H
  > >>>>>>>>>>>>>> Compiler:incoming
  > EOF

  $ combine-merge-conflicts input.ml -o output.ml
  $ diff input.ml output.ml
