(* TEST
 flags = "-I ${ocamlsrcdir}/parsing";
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
