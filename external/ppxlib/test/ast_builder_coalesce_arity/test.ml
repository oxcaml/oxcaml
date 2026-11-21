open Ppxlib

let loc = Location.none

let param name : function_param =
  {
    pparam_loc = loc;
    pparam_desc =
      Pparam_val (Nolabel, None, Ast_builder.Default.pvar ~loc name);
  }

let fn ?constraint_ params body =
  Ast_builder.Default.pexp_function ~loc (List.map param params) constraint_
    body

let show label expr =
  let string_of = Pprintast.string_of_expression in
  Printf.printf "%s:\n  before: %s\n  after:  %s\n" label (string_of expr)
    (string_of (Ast_builder.Default.coalesce_arity expr))

let () =
  show "nested functions"
    (fn [ "x" ] (Pfunction_body (fn [ "y" ] (Pfunction_body [%expr x + y]))));
  (* Our parser represents a function whose parameters are all newtypes as
     [Pexp_newtype], which upstream's [coalesce_arity] doesn't look through. *)
  show "under newtypes"
    (Ast_builder.Default.pexp_newtype ~loc { txt = "a"; loc }
       (fn [ "x" ]
          (Pfunction_body (fn [ "y" ] (Pfunction_body [%expr x + y])))));
  (* An unparenthesized [function] as the body of a [fun] is part of its
     arity. *)
  show "function cases body"
    (fn [ "x" ]
       (Pfunction_body
          (Ast_builder.Default.pexp_function_cases ~loc
             [
               Ast_builder.Default.case ~lhs:[%pat? y] ~guard:None
                 ~rhs:[%expr x + y];
             ])));
  (* Coalescing stops at a constraint on the result of the outer function,
     ... *)
  show "constrained result"
    (fn ~constraint_:(Pconstraint [%type: int -> int]) [ "x" ]
       (Pfunction_body (fn [ "y" ] (Pfunction_body [%expr x + y]))));
  (* ... and at attributes on the inner function, which we must not move. *)
  show "attributes on the body"
    (fn [ "x" ] (Pfunction_body [%expr (fun y -> x + y) [@inline]]))
