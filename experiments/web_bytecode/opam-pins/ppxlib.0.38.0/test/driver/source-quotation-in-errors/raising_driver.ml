open Ppxlib

let rules =
  [
    Extension.V3.declare "raise" Extension.Context.expression
      Ast_pattern.(pstr nil)
      (fun ~ctxt ->
        let loc = Expansion_context.Extension.extension_point_loc ctxt in
        Location.raise_errorf ~loc "An exception, raise be!")
    |> Context_free.Rule.extension;
  ]

let () = Driver.V2.register_transformation ~rules "raise"
let () = Driver.standalone ()
