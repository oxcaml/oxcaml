let () = Printexc.record_backtrace false

open Ppxlib

module N = Ppxlib_private.Name
[%%expect{|
module N = Ppxlib.Ppxlib_private.Name
|}]


let dot_suffixes name =
  Printf.sprintf "%s"
    (Sexplib0.Sexp.to_string_hum
       (Sexplib0.Sexp_conv.sexp_of_list Sexplib0.Sexp_conv.sexp_of_string (N.dot_suffixes name)))
[%%expect{|
val dot_suffixes : string -> string = <fun>
|}]

let _ = dot_suffixes "foo.bar.baz"
[%%expect{|
- : string = "(baz bar.baz foo.bar.baz)"
|}]

let _ = dot_suffixes "foo.@bar.baz"
[%%expect{|
- : string = "(bar.baz foo.bar.baz)"
|}]


let split_path name =
    let a, b = N.split_path name in
    Printf.sprintf "%s"
      (Sexplib0.Sexp.to_string_hum
         (List [Sexplib0.Sexp_conv.sexp_of_string a; Sexplib0.Sexp_conv.sexp_of_option Sexplib0.Sexp_conv.sexp_of_string b]))
[%%expect{|
val split_path : string -> string = <fun>
|}]

let _ = split_path "a.b.c"
[%%expect{|
- : string = "(a.b.c ())"
|}]

let _ = split_path "a.b.c.D"
[%%expect{|
- : string = "(a.b.c (D))"
|}]

let _ = split_path ".D"
[%%expect{|
- : string = "(\"\" (D))"
|}]

let convert_longident string =
  let lident = Longident.parse string in
  let name = Longident.name lident in
  (name, lident)
[%%expect{|
val convert_longident : string -> string * longident = <fun>
|}]

let _ = convert_longident "x"
[%%expect_in <= 5.3 {|
- : string * longident = ("x", Ppxlib.Longident.Lident "x")
|}]
[%%expect_in >= 5.4 {|
- : string * longident = ("x", Longident.Lident "x")
|}]

let _ = convert_longident "(+)"
[%%expect_in <= 5.3 {|
- : string * longident = ("( + )", Ppxlib.Longident.Lident "+")
|}]
[%%expect_in >= 5.4 {|
- : string * longident = ("( + )", Longident.Lident "+")
|}]

let _ = convert_longident "( + )"
[%%expect_in <= 5.3 {|
- : string * longident = ("( + )", Ppxlib.Longident.Lident "+")
|}]
[%%expect_in >= 5.4 {|
- : string * longident = ("( + )", Longident.Lident "+")
|}]

let _ = convert_longident "Base.x"
[%%expect_in <= 5.3 {|
- : string * longident =
("Base.x", Ppxlib.Longident.Ldot (Ppxlib.Longident.Lident "Base", "x"))
|}]
[%%expect_in >= 5.4 {|
- : string * longident =
("Base.x", Longident.Ldot (Longident.Lident "Base", "x"))
|}]

let _ = convert_longident "Base.(+)"
[%%expect_in <= 5.3 {|
- : string * longident =
("Base.( + )", Ppxlib.Longident.Ldot (Ppxlib.Longident.Lident "Base", "+"))
|}]
[%%expect_in >= 5.4 {|
- : string * longident =
("Base.( + )", Longident.Ldot (Longident.Lident "Base", "+"))
|}]

let _ = convert_longident "Base.( + )"
[%%expect_in <= 5.3 {|
- : string * longident =
("Base.( + )", Ppxlib.Longident.Ldot (Ppxlib.Longident.Lident "Base", "+"))
|}]
[%%expect_in >= 5.4 {|
- : string * longident =
("Base.( + )", Longident.Ldot (Longident.Lident "Base", "+"))
|}]

let _ = convert_longident "Base.( land )"
[%%expect_in <= 5.3 {|
- : string * longident =
("Base.( land )",
 Ppxlib.Longident.Ldot (Ppxlib.Longident.Lident "Base", "land"))
|}]
[%%expect_in >= 5.4 {|
- : string * longident =
("Base.( land )", Longident.Ldot (Longident.Lident "Base", "land"))
|}]

let _ = convert_longident "A(B)"
[%%expect_in <= 5.3 {|
Exception:
Invalid_argument "Ppxlib.Longident.parse(application in path): \"A(B)\"".
|}]
[%%expect_in >= 5.4 {|
Exception:
Invalid_argument "Ppxlib.Longident.parse(application in path): \"A(B)\"".
|}]

let _ = convert_longident "A.B(C)"
[%%expect_in <= 5.3 {|
Exception:
Invalid_argument "Ppxlib.Longident.parse(application in path): \"A.B(C)\"".
|}]
[%%expect_in >= 5.4 {|
Exception:
Invalid_argument "Ppxlib.Longident.parse(application in path): \"A.B(C)\"".
|}]

let _ = convert_longident ")"
[%%expect_in <= 5.3 {|
Exception:
Invalid_argument "Ppxlib.Longident.parse(unbalanced parenthesis): \")\"".
|}]
[%%expect_in >= 5.4 {|
Exception:
Invalid_argument "Ppxlib.Longident.parse(unbalanced parenthesis): \")\"".
|}]

let _ = convert_longident "("
[%%expect{|
Exception:
Invalid_argument "Ppxlib.Longident.parse(unbalanced parenthesis): \"(\"".
|}]

let _ = convert_longident "A.(()"
[%%expect{|
Exception:
Invalid_argument "Ppxlib.Longident.parse(unbalanced parenthesis): \"A.(()\"".
|}]

let _ = convert_longident "A.())()"
[%%expect{|
Exception:
Invalid_argument
 "Ppxlib.Longident.parse(right parenthesis misplaced): \"A.())()\"".
|}]

let _ = convert_longident "+."
[%%expect_in <= 5.3 {|
- : string * longident = ("( +. )", Ppxlib.Longident.Lident "+.")
|}]
[%%expect_in >= 5.4 {|
- : string * longident = ("( +. )", Longident.Lident "+.")
|}]

let _ = convert_longident "(+.)"
[%%expect_in <= 5.3 {|
- : string * longident = ("( +. )", Ppxlib.Longident.Lident "+.")
|}]
[%%expect_in >= 5.4 {|
- : string * longident = ("( +. )", Longident.Lident "+.")
|}]

let _ = convert_longident "Foo.(+.)"
[%%expect_in <= 5.3 {|
- : string * longident =
("Foo.( +. )", Ppxlib.Longident.Ldot (Ppxlib.Longident.Lident "Foo", "+."))
|}]
[%%expect_in >= 5.4 {|
- : string * longident =
("Foo.( +. )", Longident.Ldot (Longident.Lident "Foo", "+."))
|}]

let _ = convert_longident "Foo.( *. )"
[%%expect_in <= 5.3 {|
- : string * longident =
("Foo.( *. )", Ppxlib.Longident.Ldot (Ppxlib.Longident.Lident "Foo", "*."))
|}]
[%%expect_in >= 5.4 {|
- : string * longident =
("Foo.( *. )", Longident.Ldot (Longident.Lident "Foo", "*."))
|}]

(* Indexing operators  *)
let _ = convert_longident "(.!())"
[%%expect_in <= 5.3 {|
- : string * longident = ("( .!() )", Ppxlib.Longident.Lident ".!()")
|}]
[%%expect_in >= 5.4 {|
- : string * longident = ("( .!() )", Longident.Lident ".!()")
|}]

let _ = convert_longident "(.%(;..)<-)"
[%%expect_in <= 5.3 {|
- : string * longident =
("( .%(;..)<- )", Ppxlib.Longident.Lident ".%(;..)<-")
|}]
[%%expect_in >= 5.4 {|
- : string * longident = ("( .%(;..)<- )", Longident.Lident ".%(;..)<-")
|}]

let _ = convert_longident "Vec.(.%(;..)<-)"
[%%expect_in <= 5.3 {|
- : string * longident =
("Vec.( .%(;..)<- )",
 Ppxlib.Longident.Ldot (Ppxlib.Longident.Lident "Vec", ".%(;..)<-"))
|}]
[%%expect_in >= 5.4 {|
- : string * longident =
("Vec.( .%(;..)<- )", Longident.Ldot (Longident.Lident "Vec", ".%(;..)<-"))
|}]

let _ = Ppxlib.Code_path.(file_path @@ top_level ~file_path:"dir/main.ml")
[%%expect{|
- : string = "dir/main.ml"
|}]

let _ = Ppxlib.Code_path.(fully_qualified_path @@ top_level ~file_path:"dir/main.ml")
[%%expect{|
- : string = "Main"
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
- : string = "Main.Sub.Sub_sub.some_val"
|}]

let _ = Ppxlib.Code_path.to_string_path complex_path
[%%expect{|
- : string = "dir/main.ml.Sub.Sub_sub"
|}]

let _ =
  let a = gen_symbol () ~prefix:"__prefix__" in
  let b = gen_symbol () ~prefix:a in
  a, b
[%%expect{|
- : string * string = ("__prefix____001_", "__prefix____002_")
|}]

let _ =
  let open Ast_builder.Make (struct let loc = Location.none end) in
  let params decl =
    List.map (fun (core_type, _) -> core_type.ptyp_desc) decl.ptype_params
  in
  let decl =
    type_declaration
      ~name:{ txt = "t"; loc = Location.none }
      ~params:(List.init 3 (fun _ -> ptyp_any, (NoVariance, NoInjectivity)))
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
