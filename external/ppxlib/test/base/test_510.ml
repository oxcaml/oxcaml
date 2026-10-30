#require "base";;
#require "stdio";;

let () = Printexc.record_backtrace false

open Base
open Stdio
open Ppxlib

module N = Ppxlib_private.Name
[%%expect{|
module N = Ppxlib.Ppxlib_private.Name
|}]


let dot_suffixes name =
  Caml.Printf.sprintf "%s"
    (Sexp.to_string_hum
       (List.sexp_of_t String.sexp_of_t (N.dot_suffixes name)))
[%%expect{|
val dot_suffixes : string/2 -> string/2 = <fun>
|}]

let _ = dot_suffixes "foo.bar.baz"
[%%expect{|
- : string/2 = "(baz bar.baz foo.bar.baz)"
|}]

let _ = dot_suffixes "foo.@bar.baz"
[%%expect{|
- : string/2 = "(bar.baz foo.bar.baz)"
|}]


let split_path name =
    let a, b = N.split_path name in
    Caml.Printf.sprintf "%s"
      (Sexp.to_string_hum
         (List [sexp_of_string a; Option.sexp_of_t sexp_of_string b]))
[%%expect{|
val split_path : string/2 -> string/2 = <fun>
|}]

let _ = split_path "a.b.c"
[%%expect{|
- : string/2 = "(a.b.c ())"
|}]

let _ = split_path "a.b.c.D"
[%%expect{|
- : string/2 = "(a.b.c (D))"
|}]

let _ = split_path ".D"
[%%expect{|
- : string/2 = "(\"\" (D))"
|}]

let convert_longident string =
  let lident = Longident.parse string in
  let name = Longident.name lident in
  (name, lident)
[%%expect{|
val convert_longident : string/2 -> string/2 * longident = <fun>
|}]

let _ = convert_longident "x"
[%%expect{|
- : string/2 * longident = ("x", Ppxlib.Longident.Lident "x")
|}]

let _ = convert_longident "(+)"
[%%expect{|
- : string/2 * longident = ("( + )", Ppxlib.Longident.Lident "+")
|}]

let _ = convert_longident "( + )"
[%%expect{|
- : string/2 * longident = ("( + )", Ppxlib.Longident.Lident "+")
|}]

let _ = convert_longident "Base.x"
[%%expect{|
- : string/2 * longident =
("Base.x", Ppxlib.Longident.Ldot (Ppxlib.Longident.Lident "Base", "x"))
|}]

let _ = convert_longident "Base.(+)"
[%%expect{|
- : string/2 * longident =
("Base.( + )", Ppxlib.Longident.Ldot (Ppxlib.Longident.Lident "Base", "+"))
|}]

let _ = convert_longident "Base.( + )"
[%%expect{|
- : string/2 * longident =
("Base.( + )", Ppxlib.Longident.Ldot (Ppxlib.Longident.Lident "Base", "+"))
|}]

let _ = convert_longident "Base.( land )"
[%%expect{|
- : string/2 * longident =
("Base.( land )",
 Ppxlib.Longident.Ldot (Ppxlib.Longident.Lident "Base", "land"))
|}]

let _ = convert_longident "A(B)"
[%%expect{|
Exception: Invalid_argument "Ppxlib.Longident.parse: \"A(B)\"".
|}]

let _ = convert_longident "A.B(C)"
[%%expect{|
Exception: Invalid_argument "Ppxlib.Longident.parse: \"A.B(C)\"".
|}]

let _ = convert_longident ")"
[%%expect{|
Exception: Invalid_argument "Ppxlib.Longident.parse: \")\"".
|}]

let _ = Ppxlib.Code_path.(file_path @@ top_level ~file_path:"dir/main.ml")
[%%expect{|
- : string/2 = "dir/main.ml"
|}]

let _ = Ppxlib.Code_path.(fully_qualified_path @@ top_level ~file_path:"dir/main.ml")
[%%expect{|
- : string/2 = "Main"
|}]

let complex_path =
  let open Ppxlib.Code_path in
  let loc = Ppxlib.Location.none in
  top_level ~file_path:"dir/main.ml"
  |> enter_module ~loc "Sub"
  |> enter_module ~loc "Sub_sub"
  |> enter_value ~loc "some_val"
[%%expect{|
val complex_path : Code_path.t = <abstr>
|}]

let _ = Ppxlib.Code_path.fully_qualified_path complex_path
[%%expect{|
- : string/2 = "Main.Sub.Sub_sub.some_val"
|}]

let _ = Ppxlib.Code_path.to_string_path complex_path
[%%expect{|
- : string/2 = "dir/main.ml.Sub.Sub_sub"
|}]

let _ =
  let a = gen_symbol () ~prefix:"__prefix__" in
  let b = gen_symbol () ~prefix:a in
  a, b
[%%expect{|
- : string/2 * string/2 = ("__prefix____001_", "__prefix____002_")
|}]

let _ =
  let open Ast_builder.Make (struct let loc = Location.none end) in
  let params decl =
    List.map decl.ptype_params ~f:(fun (core_type, _) -> core_type.ptyp_desc)
  in
  let decl =
    type_declaration
      ~name:{ txt = "t"; loc = Location.none }
      ~params:(List.init 3 ~f:(fun _ -> ptyp_any, (NoVariance, NoInjectivity)))
      ~cstrs:[]
      ~kind:Ptype_abstract
      ~private_:Public
      ~manifest:None
  in
  params decl, params (name_type_params_in_td decl)
[%%expect{|
- : core_type_desc list * core_type_desc list =
([Ptyp_any; Ptyp_any; Ptyp_any],
 [Ptyp_var "a__003_"; Ptyp_var "b__004_"; Ptyp_var "c__005_"])
|}]
