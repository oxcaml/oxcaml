open Import

type affix =
  | Prefix of string
  | Suffix of string
  | PrefixSuffix of string * string

let mangle ?(fixpoint = "t") affix name =
  match (String.(name = fixpoint), affix) with
  | true, (Prefix x | Suffix x) -> x
  | true, PrefixSuffix (p, s) -> p ^ "_" ^ s
  | false, PrefixSuffix (p, s) -> p ^ "_" ^ name ^ "_" ^ s
  | false, Prefix x -> x ^ "_" ^ name
  | false, Suffix x -> name ^ "_" ^ x

let mangle_type_decl ?fixpoint affix { ptype_name = { txt = name; _ }; _ } =
  mangle ?fixpoint affix name

let mangle_lid ?fixpoint affix lid =
  match lid with
  | Lident s -> Lident (mangle ?fixpoint affix s)
  | Ldot (p, s) -> Ldot (p, mangle ?fixpoint affix s)
  | Lapply _ -> invalid_arg "Ppxlib.Expansion_helpers.mangle_lid: Lapply"

module Quoter = Quoter
