module To_before_503 =
  Ppxlib_ast.Convert (Ppxlib_ast.Js) (Ppxlib_ast__.Versions.OCaml_502)

module From_before_503 =
  Ppxlib_ast.Convert (Ppxlib_ast__.Versions.OCaml_502) (Ppxlib_ast.Js)

let impl _ctxt str =
  (* This manual migration is here to ensure the test still works even once our
     internal AST has been bumped past 5.3 *)
  let before_503_ast = To_before_503.copy_structure str in
  let roundtrip = From_before_503.copy_structure before_503_ast in
  roundtrip

let () = Ppxlib.Driver.V2.register_transformation ~impl "503-downward-roundtrip"
let () = Ppxlib.Driver.standalone ()
