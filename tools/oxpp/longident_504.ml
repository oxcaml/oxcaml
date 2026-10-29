(* [Ast_504] refers to [Longident_504.t]; OxCaml's [Longident.t] has exactly the
   shape of OCaml 5.4's, so equate the two. *)
type t = Longident.t =
  | Lident of string
  | Ldot of t Location.loc * string Location.loc
  | Lapply of t Location.loc * t Location.loc
