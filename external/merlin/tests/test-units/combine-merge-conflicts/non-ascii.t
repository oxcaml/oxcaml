  $ cat > input.ml <<EOF
  > <<<<<<<<<<<<<< Merlin:current
  > let s = "café"
  > |||||||||||||| Compiler:last-imported
  > let s = "café"
  > ==============
  > let s = "thé"
  > >>>>>>>>>>>>>> Compiler:incoming
  > EOF

  $ combine-merge-conflicts input.ml -o output.ml
  $ diff input.ml output.ml

  $ LC_ALL=C combine-merge-conflicts input.ml -o output.ml
  $ diff input.ml output.ml
