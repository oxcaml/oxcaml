An inner conflict split across three outer conflicts.

  $ cat > input.ml <<EOF
  > <<<<<<<<<<<<<< Merlin:current
  > <<<<<<< current
  > A
  > |||||||||||||| Compiler:last-imported
  > <<<<<<< current
  > B
  > ==============
  > <<<<<<< current
  > C
  > >>>>>>>>>>>>>> Compiler:incoming
  > D
  > <<<<<<<<<<<<<< Merlin:current
  > E
  > =======
  > F
  > |||||||||||||| Compiler:last-imported
  > G
  > ==============
  > H
  > >>>>>>>>>>>>>> Compiler:incoming
  > I
  > <<<<<<<<<<<<<< Merlin:current
  > J
  > >>>>>>> incoming
  > |||||||||||||| Compiler:last-imported
  > K
  > =======
  > L
  > >>>>>>> incoming
  > ==============
  > M
  > =======
  > N
  > >>>>>>> incoming
  > >>>>>>>>>>>>>> Compiler:incoming
  > EOF

  $ combine-merge-conflicts input.ml -o output.ml
  $ cat output.ml
  <<<<<<<<<<<<<< Merlin:current
  <<<<<<< current
  A
  D
  E
  =======
  F
  I
  J
  >>>>>>> incoming
  |||||||||||||| Compiler:last-imported
  <<<<<<< current
  B
  D
  G
  I
  K
  =======
  L
  >>>>>>> incoming
  ==============
  <<<<<<< current
  C
  D
  H
  I
  M
  =======
  N
  >>>>>>> incoming
  >>>>>>>>>>>>>> Compiler:incoming
