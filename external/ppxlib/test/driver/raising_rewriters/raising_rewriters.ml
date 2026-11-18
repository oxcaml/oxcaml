open Ppxlib

(* [[@@deriving nothing]] generates no code. *)
let (_ : Deriving.t) =
  let nothing ~loc:_ ~path:_ _ = [] in
  Deriving.add "nothing"
    ~str_type_decl:(Deriving.Generator.make_noarg nothing)
    ~sig_type_decl:(Deriving.Generator.make_noarg nothing)

(* [[@@deriving raise]] raises. *)
let (_ : Deriving.t) =
  let raise_ ~loc ~path:_ _ = Location.raise_errorf ~loc "the deriver raised" in
  Deriving.add "raise"
    ~str_type_decl:(Deriving.Generator.make_noarg raise_)
    ~sig_type_decl:(Deriving.Generator.make_noarg raise_)

(* [[%raise]] raises. *)
let () =
  let raise_ ~loc ~path:_ = Location.raise_errorf ~loc "the extension raised" in
  Driver.register_transformation "raise"
    ~rules:
      [
        Context_free.Rule.extension
          (Extension.declare "raise" Extension.Context.expression
             Ast_pattern.(pstr nil)
             raise_);
      ]

(* A whole-file pass, which runs after the derivers and the extensions: it removes the
   bindings marked [[@@drop]]. *)
let () =
  let is_drop attribute =
    String.equal attribute.attr_name.txt "drop"
    &&
    (Attribute.mark_as_handled_manually attribute;
     true)
  in
  let dropped item =
    match item.pstr_desc with
    | Pstr_value (_, bindings) ->
        List.exists
          (fun binding -> List.exists is_drop binding.pvb_attributes)
          bindings
    | _ -> false
  in
  Driver.register_transformation "drop"
    ~impl:(List.filter (fun item -> not (dropped item)))

let () = Driver.standalone ()
