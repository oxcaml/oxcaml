Unrelated inner and outer merge conflicts.

  $ cat > input.ml <<'EOF'
  > <<<<<<< current
  > A
  > ||||||| merge-base
  > B
  > =======
  > C
  > >>>>>>> incoming
  > D
  > <<<<<<<<<<<<<< Merlin:current
  > E
  > |||||||||||||| Compiler:last-imported
  > F
  > ==============
  > G
  > >>>>>>>>>>>>>> Compiler:incoming
  > EOF

  $ combine-merge-conflicts input.ml -o output.ml
  $ diff input.ml output.ml
