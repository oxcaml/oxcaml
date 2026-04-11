module To_before_502 =
  Ppxlib_ast.Convert (Ppxlib_ast.Js) (Ppxlib_ast__.Versions.OCaml_501)

module From_before_502 =
  Ppxlib_ast.Convert (Ppxlib_ast__.Versions.OCaml_501) (Ppxlib_ast.Js)

let impl _ctxt str =
  (* This manual migration is here to ensure the test still works even once our
     internal AST has been bumped past 5.3 *)
  let before_502_ast = To_before_502.copy_structure str in
  let roundtrip = From_before_502.copy_structure before_502_ast in
  roundtrip

let () = Ppxlib.Driver.V2.register_transformation ~impl "502-downward-roundtrip"
let () = Ppxlib.Driver.standalone ()
