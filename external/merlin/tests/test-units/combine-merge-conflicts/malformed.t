If merge conflicts don't line up correctly, exit with error code 21 and don't
write any changes.

  $ cat > input.ml <<EOF
  > <<<<<<<<<<<<<< Merlin:current
  > A
  > |||||||||||||| Compiler:last-imported
  > B
  > ==============
  > C
  > EOF
  $ cp input.ml old_input.ml
  $ combine-merge-conflicts input.ml -o output.ml 2> error.txt
  [21]
  $ diff old_input.ml input.ml

  $ cat > input.ml <<EOF
  > <<<<<<< current
  > A
  > ||||||| incoming
  > C
  > EOF
  $ cp input.ml old_input.ml
  $ combine-merge-conflicts input.ml -o output.ml 2> error.txt
  [21]
  $ diff old_input.ml input.ml

  $ cat > input.ml <<EOF
  > <<<<<<<<<<<<<< Merlin:current
  > <<<<<<< current
  > A
  > |||||||||||||| Compiler:last-imported
  > B
  > ==============
  > C
  > >>>>>>>>>>>>>> Compiler:incoming
  > ||||||| merge-base
  > D
  > =======
  > E
  > >>>>>>> merlin-branch
  > <<<<<<<<<<<<<< Merlin:current
  > F
  > |||||||||||||| Compiler:last-imported
  > G
  > ==============
  > H
  > >>>>>>>>>>>>>> Compiler:incoming
  > EOF
  $ cp input.ml old_input.ml
  $ combine-merge-conflicts input.ml -o output.ml 2> error.txt
  [21]
  $ diff old_input.ml input.ml
