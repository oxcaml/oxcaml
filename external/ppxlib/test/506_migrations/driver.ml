module To_before_506 =
  Ppxlib_ast.Convert (Ppxlib_ast.Js) (Ppxlib_ast__.Versions.OCaml_505)

module From_before_506 =
  Ppxlib_ast.Convert (Ppxlib_ast__.Versions.OCaml_505) (Ppxlib_ast.Js)

let impl _ctxt str =
  (* This manual migration is here to ensure the test still works even once our
     internal AST has been bumped past 5.6 *)
  let before_506_ast = To_before_506.copy_structure str in
  let roundtrip = From_before_506.copy_structure before_506_ast in
  roundtrip

let () = Ppxlib.Driver.V2.register_transformation ~impl "506-downward-roundtrip"
let () = Ppxlib.Driver.standalone ()
