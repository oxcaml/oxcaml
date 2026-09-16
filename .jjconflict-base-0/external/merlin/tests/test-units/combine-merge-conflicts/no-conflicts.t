  $ cat > input.ml <<EOF
  > let x = 1
  > let y = 2
  > EOF

  $ combine-merge-conflicts input.ml -o output.ml
  $ diff input.ml output.ml
