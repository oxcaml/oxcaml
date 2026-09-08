open! Astlib
open! Ppxlib_ast.Asttypes
open! Ppxlib_ast.Parsetree

(** Use of this module is discouraged.

    This module allows you to turn matches on [Pexp_function (params, constraint_, body)]
    into matches on the old parsetree prior to OCaml 5.2:
    - [Pexp_newtype (newtype, body)]
    - [Pexp_fun (label, default, pat)]
    - [Pexp_function cases]

    These constructors are available below, but with [Legacy_] tacked onto the name.

    Generally, ppx authors should prefer handling the new [Pexp_function] constructor
    directly. This module was introduced to make it easier to update old ppxes, but it has
    less obvious behavior and is kludgy. *)

type legacy_pexp_fun := arg_label * expression option * pattern * expression
type legacy_pexp_function := case list
type legacy_pexp_newtype := string loc * expression

type t =
  | Legacy_pexp_fun of legacy_pexp_fun
  (** Match [Pexp_fun], as in the OCaml parsetree prior to 5.2. To construct, use
      [Ppxlib.Ast_builder.Default.pexp_fun]. *)
  | Legacy_pexp_function of legacy_pexp_function
  (** Match [Pexp_function], as in the OCaml parsetree prior to 5.2. To construct, use
      [Ppxlib.Ast_builder.Default.pexp_function]. *)
  | Legacy_pexp_newtype of legacy_pexp_newtype

(** Creation functions *)

val of_parsetree : expression_desc -> t option

val of_pexp_function
  :  params:Shim.Pexp_function.function_param list
  -> constraint_:Shim.Pexp_function.Function_constraint.t
  -> body:Shim.Pexp_function.function_body
  -> t

(** Testing a particular constructor of [t]. *)

val legacy_pexp_fun_of_parsetree : expression_desc -> legacy_pexp_fun option
val legacy_pexp_function_of_parsetree : expression_desc -> legacy_pexp_function option
val legacy_pexp_newtype_of_parsetree : expression_desc -> legacy_pexp_newtype option
