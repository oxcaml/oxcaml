open Ppxlib
open Ast_builder.Default

(* [[@@deriving nested_jkind]] expands to
   [type nested : value_or_null non_null non_float], built as two nested
   [Pjk_operator]s, as a ppx substituting [value_or_null non_null] for [k] in
   [k non_float] would. The parser reads it back as a single one. *)
let (_ : Deriving.t) =
  let str_type_decl =
    Deriving.Generator.make_noarg (fun ~loc ~path:_ _ ->
        let jkind pjka_desc = { pjka_loc = loc; pjka_desc } in
        let value_or_null =
          jkind (Pjk_abbreviation { loc; txt = Lident "value_or_null" })
        in
        let inner =
          jkind (Pjk_operator (value_or_null, [ { loc; txt = "non_null" } ]))
        in
        let outer = jkind (Pjk_operator (inner, [ { loc; txt = "non_float" } ])) in
        let td =
          type_declaration ~loc ~name:{ loc; txt = "nested" } ~params:[]
            ~cstrs:[] ~kind:Ptype_abstract ~private_:Public ~manifest:None
        in
        [
          pstr_type ~loc Recursive
            [ { td with ptype_jkind_annotation = Some outer } ];
        ])
  in
  Deriving.add "nested_jkind" ~str_type_decl
