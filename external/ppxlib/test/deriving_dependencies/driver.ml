open! Ppxlib

let register_deriver ?deps name f =
  let str_type_decl =
    Deriving.Generator.make ?deps
      Deriving.Args.(empty +> flag name)
      (fun ~loc ~path:_ _ast arg ->
        f ~loc (if arg then [%expr true] else [%expr false]))
  in
  Deriving.add name ~str_type_decl

let foo =
  register_deriver "foo" (fun ~loc arg ->
      [%str
        let (`Foo _) =
          let module Foo = struct end in
          `Foo [%e arg]])

let bar =
  register_deriver "bar" (fun ~loc arg ->
      [%str
        let (`Bar _) =
          let module Bar = struct end in
          `Bar [%e arg]])

let use_foo = register_deriver "use_foo" ~deps:[ foo ] (fun ~loc:_ _arg -> [])
let () = Deriving.add_alias "foo_bar" [ foo; bar ] |> Deriving.ignore

(* TODO: It's unfortunate that dependency-checking is done when the ppx is
   applied, because then the error is given to the user. I would prefer to fail
   as soon as the alias is registered. *)
let () =
  Deriving.add_alias "dep_before_use" [ foo; use_foo ] |> Deriving.ignore

let () =
  Deriving.add_alias "use_before_dep" [ use_foo; foo ] |> Deriving.ignore

let () =
  let str_type_decl =
    Deriving.Generator.make Deriving.Args.empty
      (fun ~loc:_ ~path:_ (_rec_flag, tds) ->
        match tds with
        | [ td ] ->
            let td = name_type_params_in_td td in
            let () =
              List.iter
                (fun (ct, _) ->
                  List.iter
                    (fun attr -> Attribute.mark_as_handled_manually attr)
                    ct.ptyp_attributes)
                td.ptype_params
            in
            [%str]
        | _ -> failwith "[baz] expected one td")
  in
  Deriving.add "baz" ~str_type_decl |> Deriving.ignore

let () = Driver.standalone ()
