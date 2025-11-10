(* TEST
 flags = "-I ${ocamlsrcdir}/parsing -I ${ocamlsrcdir}/toplevel";
 include ocamlcommon;
 expect;
*)

let test_impl str =
  let lexbuf = Lexing.from_string str in
  Lexing.set_filename lexbuf "file.ml";
  match Parse.implementation lexbuf with
  | [ { pstr_loc; pstr_desc = Pstr_exception { ptyexn_loc; ptyexn_constructor = { pext_loc; pext_name = { loc = pext_name_loc; _ }; _ } }; _ } ] ->
    let fmt_loc = Location.print_loc in
    Format.printf "Structure item loc: %a@." fmt_loc pstr_loc;
    Format.printf "Exception declaration loc: %a@." fmt_loc ptyexn_loc;
    Format.printf "Exception constructor loc: %a@." fmt_loc pext_loc;
    Format.printf "Exception constructor name loc: %a@." fmt_loc pext_name_loc;
    ()
  | _ -> assert false

[%%expect {|
val test_impl : string -> unit = <fun>
|}]

let () = test_impl "exception E of int"

[%%expect {|
Structure item loc: File "file.ml", line 1, characters 0-18
Exception declaration loc: File "_none_", line 1
Exception constructor loc: File "file.ml", line 1, characters 0-18
Exception constructor name loc: File "file.ml", line 1, characters 10-11
|}]

let () = test_impl "exception E = F"

[%%expect {|
Structure item loc: File "file.ml", line 1, characters 0-15
Exception declaration loc: File "_none_", line 1
Exception constructor loc: File "file.ml", line 1, characters 0-15
Exception constructor name loc: File "file.ml", line 1, characters 10-11
|}]

let () = test_impl "exception E of int [@@deriving sexp]"

[%%expect {|
Structure item loc: File "file.ml", line 1, characters 0-36
Exception declaration loc: File "_none_", line 1
Exception constructor loc: File "file.ml", line 1, characters 0-18
Exception constructor name loc: File "file.ml", line 1, characters 10-11
|}]
