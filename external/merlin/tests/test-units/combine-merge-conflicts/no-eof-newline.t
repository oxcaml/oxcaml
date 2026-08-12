If there's no newline at the end of the input file, then there's none in the
output file.

  $ printf '%s' 'foo' > input.ml

  $ combine-merge-conflicts input.ml -o output.ml
  $ cat output.ml; echo '[eof]'
  foo[eof]

If there's a terminal merge conflict, then we still add the newline.

  $ printf '%s' '<<<<<<<<<<<<<< Merlin:current
  > A
  > |||||||||||||| Compiler:last-imported
  > B
  > ==============
  > C
  > >>>>>>>>>>>>>> Compiler:incoming' > input.ml

  $ combine-merge-conflicts input.ml -o output.ml
  $ cat output.ml; echo '[eof]'
  <<<<<<<<<<<<<< Merlin:current
  A
  |||||||||||||| Compiler:last-imported
  B
  ==============
  C
  >>>>>>>>>>>>>> Compiler:incoming
  [eof]
