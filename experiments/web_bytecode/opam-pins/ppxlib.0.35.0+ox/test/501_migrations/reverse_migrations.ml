module Reverse = Ppxlib_ast.Select_ast (Ppxlib_ast__.Versions.OCaml_501)

let () =
  let impl str =
    Reverse.Of_ocaml.copy_structure @@ Reverse.To_ocaml.copy_structure str
  in
  Ppxlib.Driver.register_transformation ~impl "reverse_migrations"

let () = Ppxlib.Driver.standalone ()
