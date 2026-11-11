open Ppxlib

(* This file contains tests to ensure that [Ast_builder.value_binding] properly
   translates the given [pattern] and [expression] pair into the correct
   [pattern], [expression] and [value_constraint] triple. *)


(* ------- Test Setup -------- *)

#install_printer Pp_ast.Default.structure_item;;
#install_printer Pp_ast.Default.expression;;
#install_printer Pp_ast.Default.pattern;;

let loc = Location.none
[%%ignore]

(* --------- Simple case, no translation --------- *)

let pat = [%pat? f]
let expr = [%expr fun x -> x + 1]
[%%ignore]

let vb =
  let open Ast_builder.Default in
  pstr_value ~loc Nonrecursive [value_binding ~pat ~expr ~loc]

[%%expect{|
val vb : structure_item =
  Pstr_value
    ( Nonrecursive
    , [ { pvb_pat = Ppat_var "f"
        ; pvb_expr =
            Pexp_function
              ( [ { pparam_loc = __loc
                  ; pparam_desc = Pparam_val ( Nolabel, None, Ppat_var "x")
                  }
                ]
              , None
              , Pfunction_body
                  (Pexp_apply
                     ( Pexp_ident (Lident "+")
                     , [ ( Nolabel, Pexp_ident (Lident "x"))
                       ; ( Nolabel
                         , Pexp_constant (Pconst_integer ( "1", None))
                         )
                       ]
                     ))
              )
        ; pvb_constraint = None
        ; pvb_attributes = __attrs
        ; pvb_loc = __loc
        }
      ]
    )
|}]

(* As expected here, the [pvb_constraint] field is none, the pattern and
   expression are used as is. *)

(* --------- No var Ppat_constraint to pvb_constraint --------- *)

let pat = [%pat? (x : int)]
let expr = [%expr 12]
[%%ignore]

let vb =
  let open Ast_builder.Default in
  pstr_value ~loc Nonrecursive [value_binding ~pat ~expr ~loc]

[%%expect{|
val vb : structure_item =
  Pstr_value
    ( Nonrecursive
    , [ { pvb_pat = Ppat_var "x"
        ; pvb_expr = Pexp_constant (Pconst_integer ( "12", None))
        ; pvb_constraint =
            Some
              (Pvc_constraint
                 { locally_abstract_univars = []
                 ; typ = Ptyp_constr ( Lident "int", [])
                 })
        ; pvb_attributes = __attrs
        ; pvb_loc = __loc
        }
      ]
    )
|}]

(* --------- poly Ppat_constraint to pvb_constraint --------- *)

let pat =
  Ast_builder.Default.ppat_constraint ~loc
    [%pat? f]
    (Ast_builder.Default.ptyp_poly ~loc
       [ Loc.make ~loc "a" ]
       [%type: 'a -> unit])

let expr = [%expr fun x -> unit]

[%%ignore]

let vb =
  let open Ast_builder.Default in
  pstr_value ~loc Nonrecursive [value_binding ~pat ~expr ~loc]

[%%expect{|
val vb : structure_item =
  Pstr_value
    ( Nonrecursive
    , [ { pvb_pat = Ppat_var "f"
        ; pvb_expr =
            Pexp_function
              ( [ { pparam_loc = __loc
                  ; pparam_desc = Pparam_val ( Nolabel, None, Ppat_var "x")
                  }
                ]
              , None
              , Pfunction_body (Pexp_ident (Lident "unit"))
              )
        ; pvb_constraint =
            Some
              (Pvc_constraint
                 { locally_abstract_univars = []
                 ; typ =
                     Ptyp_poly
                       ( [ "a"]
                       , Ptyp_arrow
                           ( Nolabel
                           , Ptyp_var "a"
                           , Ptyp_constr ( Lident "unit", [])
                           )
                       )
                 })
        ; pvb_attributes = __attrs
        ; pvb_loc = __loc
        }
      ]
    )
|}]

(* --------- desugared locally abstract univars to pvb_constraint --------- *)

let pat =
  Ast_builder.Default.ppat_constraint ~loc
    [%pat? f]
    (Ast_builder.Default.ptyp_poly ~loc
       [ Loc.make ~loc "a" ]
       [%type: 'a -> unit])

let expr = [%expr fun (type a) -> (fun _ -> unit : a -> unit)]

[%%ignore]

let vb =
  let open Ast_builder.Default in
  pstr_value ~loc Nonrecursive [value_binding ~pat ~expr ~loc]

[%%expect{|
val vb : structure_item =
  Pstr_value
    ( Nonrecursive
    , [ { pvb_pat = Ppat_var "f"
        ; pvb_expr =
            Pexp_function
              ( [ { pparam_loc = __loc
                  ; pparam_desc = Pparam_val ( Nolabel, None, Ppat_any)
                  }
                ]
              , None
              , Pfunction_body (Pexp_ident (Lident "unit"))
              )
        ; pvb_constraint =
            Some
              (Pvc_constraint
                 { locally_abstract_univars = [ "a"]
                 ; typ =
                     Ptyp_arrow
                       ( Nolabel
                       , Ptyp_constr ( Lident "a", [])
                       , Ptyp_constr ( Lident "unit", [])
                       )
                 })
        ; pvb_attributes = __attrs
        ; pvb_loc = __loc
        }
      ]
    )
|}]

(* As expected here, the matching constraint from the pattern and expression or
   recombined into a single value constraint with locally abstract univars set
   correctly. *)

(* --------- coercion to pvb_constraint --------- *)

(*TODO*)
[%%expect{|
|}]
