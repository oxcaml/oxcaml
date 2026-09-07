  $ cat > input.ml <<EOF
  > let x = 1
  > <<<<<<< current
  > let y = 2
  > ||||||| merge-base
  > let y = 1
  > =======
  > let y = 3
  > >>>>>>> incoming
  > let z = 4
  > EOF

  $ combine-merge-conflicts input.ml -o output.ml
  $ diff input.ml output.ml
