The driver can read from stdin.

It works with sources...

  $ ../identity_driver.exe -impl - << EOF
  > let a = 1
  > EOF
  let a = 1

... but it should also work with binary ASTs.
We generate a binary AST file...

  $ ../identity_driver.exe --dump-ast -o binary_ast -impl - << EOF
  > let b = 2
  > EOF

... and ensure the driver can also read it from stdin

  $ cat binary_ast | ../identity_driver.exe -impl -
  let b = 2
