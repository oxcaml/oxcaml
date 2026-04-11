open Ppxlib

module B = Ast_builder.Make (struct
  let loc = Location.none
end)

let add_function =
  let param v = B.pparam_val Nolabel None (B.ppat_var (B.Located.mk v)) in
  let body =
    B.pexp_function
      [ param "b" ]
      None
      (Pfunction_body
         (B.pexp_apply
            (B.pexp_ident (B.Located.mk (Lident "+")))
            [
              (Nolabel, B.pexp_ident (B.Located.lident "a"));
              (Nolabel, B.pexp_ident (B.Located.lident "b"));
            ]))
  in
  let func = B.pexp_function [ param "a" ] None (Pfunction_body body) in
  B.pstr_value Nonrecursive
    [ B.value_binding ~pat:(B.ppat_var (B.Located.mk "f")) ~expr:func ]

let generative_functor =
  let empty_struct = B.pmod_structure [] in
  B.pstr_module
    (B.module_binding ~name:(B.Located.mk (Some "F"))
       ~expr:(B.pmod_apply (B.pmod_ident (B.Located.lident "F")) empty_struct))

let () =
  Driver.register_transformation
    ~impl:(fun impl -> add_function :: generative_functor :: impl)
    "migrators"

let () = Driver.standalone ()
