open Ppxlib
open Ast_builder.Default

(* [[%rebuild_letop let* x = e in body]] rebuilds the [let*] from scratch, giving
   the binding operator the location of the whole expression, as the parser
   does. *)
let rebuild_letop =
  Extension.V3.declare "rebuild_letop" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~ctxt:_ e ->
      match e.pexp_desc with
      | Pexp_letop { let_; ands = []; body } ->
          let loc = e.pexp_loc in
          let let_ =
            binding_op ~loc ~op:let_.pbop_op ~pat:let_.pbop_pat
              ~exp:let_.pbop_exp
          in
          pexp_letop ~loc (letop ~let_ ~ands:[] ~body)
      | _ -> Location.raise_errorf ~loc:e.pexp_loc "expected a let*")

let () =
  Driver.register_transformation "location_check"
    ~rules:[ Context_free.Rule.extension rebuild_letop ]

let () = Driver.standalone ()
