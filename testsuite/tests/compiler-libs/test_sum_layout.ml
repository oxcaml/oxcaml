(* TEST
 flags = "-I ${ocamlsrcdir}/parsing -I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/utils";
 include ocamlcommon;
 expect;
*)

let print_layout source =
  let parse s = Parse.interface (Lexing.from_string s) in
  let print ast = Format.asprintf "%a" Pprintast.signature ast in
  let ast = parse ("type t : " ^ source) in
  let printed = print ast in
  let mapper = { Ast_mapper.default_mapper with
    location = (fun _ _ -> Location.none)
  } in
  let normalize = mapper.signature mapper in
  assert (normalize ast = normalize (parse printed));
  Format.printf "%s@." printed
;;

[%%expect{|
val print_layout : string -> unit = <fun>
|}];;

List.iter print_layout
  [ "bits32 & bits32 + void + value"
  ; "(bits32 + value) & bits64"
  ; "bits32 + (void + value)"
  ];;

[%%expect{|
type t : (bits32 & bits32) + void + value
type t : (bits32 + value) & bits64
type t : bits32 + (void + value)
- : unit = ()
|}];;

let print_constant_kind source =
  let structure = Parse.implementation (Lexing.from_string source) in
  let _, _, _, _, _, env =
    Typemod.type_structure (Lazy.force Env.initial) structure
  in
  let _, decl = Env.find_type_by_name (Longident.Lident "t") env in
  let kind = Jkind.get decl.type_jkind |> Jkind.Desc.get_const |> Option.get in
  let printed = Jkind.Const.to_out_jkind_const env kind in
  Format_doc.asprintf "%a" !Oprint.out_jkind_const printed
;;

[%%expect{|
val print_constant_kind : string -> string = <fun>
|}];;

print_constant_kind "type t : (value + bits32) mod portable";;
print_constant_kind "type 'a t : (bits32 + bits64) with 'a";;

[%%expect{|
- : string = "(value + bits32) mod portable"
- : string = "(bits32 + bits64) with 'a"
|}];;
