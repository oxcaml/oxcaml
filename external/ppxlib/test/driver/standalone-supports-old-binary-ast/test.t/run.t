Binary AST's of any by ppxlib supported OCaml version are supported.
The version is preserved.

  $ cat 408_binary_ast | ../print_magic_number.exe
  Magic number: Caml1999N025

  $ ../identity_standalone.exe --intf 408_binary_ast -o transformed --dump-ast

  $ ../print_magic_number.exe < transformed
  Magic number: Caml1999N025
