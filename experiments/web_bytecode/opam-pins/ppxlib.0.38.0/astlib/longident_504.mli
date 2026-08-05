type t =
  (*IF_AT_LEAST 504 Ocaml_common.Longident.t = *)
  | Lident of string
  | Ldot of t Location.loc * string Location.loc
  | Lapply of t Location.loc * t Location.loc
