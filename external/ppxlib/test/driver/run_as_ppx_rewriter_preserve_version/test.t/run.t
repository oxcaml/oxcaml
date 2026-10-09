Binary AST's of any by ppxlib supported OCaml version are supported.
The version is preserved.

  $ cat 408_binary_ast | ../print_magic_number.exe
  Magic number: Caml1999M025

  $ ../identity_standalone.exe 408_binary_ast /dev/stdout | ../print_magic_number.exe
  Magic number: Caml1999M025
