open Ppxlib

(* [[@@deriving gensym]] generates a binding with a generated name. *)
let (_ : Deriving.t) =
  let gensym ~loc ~path:_ _ =
    let open Ast_builder.Default in
    [ pstr_value ~loc Nonrecursive
        [ value_binding ~loc
            ~pat:(ppat_var ~loc { loc; txt = gen_symbol ~prefix:"x" () })
            ~expr:(eunit ~loc) ] ]
  in
  Deriving.add "gensym" ~str_type_decl:(Deriving.Generator.make_noarg gensym)

let () = Driver.standalone ()
