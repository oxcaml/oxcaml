(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open! Js_of_ocaml_compiler
open! Stdlib
open! Util

[@@@if oxcaml]

let%expect_test "Var.set_last never lowers the counter" =
  Code.Var.reset ();
  let fresh () = Printf.printf "%d\n" (Code.Var.idx (Code.Var.fresh ())) in
  Code.Var.set_last 10;
  fresh ();
  Code.Var.set_last 5;
  fresh ();
  Code.Var.set_last 20;
  fresh ();
  [%expect {|
    11
    12
    21
    |}]

let%expect_test "caml_get_symbol reports unregistered compilation units and symbols" =
  compile_and_run
    {|
external register : string -> string -> 'a -> unit = "caml_register_symbol"
external get : string -> string -> 'a = "caml_get_symbol"

let lookup cu sym =
  match (get cu sym : int) with
  | n -> Printf.printf "%s.%s = %d\n" cu sym n
  | exception Invalid_argument msg -> print_endline msg

let () =
  register "Foo" "x" 42;
  lookup "Foo" "x";
  lookup "Foo" "y";
  lookup "Bar" "x"
|};
  [%expect
    {|
    Foo.x = 42
    caml_get_symbol: compilation unit Foo does not define symbol y
    caml_get_symbol: compilation unit Bar has not been initialised yet (looking up x)
    |}]

(* Synthetic JSIR files, laid out as [ocamlopt -target js_of_ocaml] produces
   them. The programs are built directly in JSIR, as that pipeline is not
   available from these tests. *)

let string s = Code.Pc (Code.String s)

let native_string s = Code.Pc (Code.NativeString (Code.Native_string.of_string s))

let extern name args = Code.Prim (Code.Extern (name, None), args)

let jsir_file ?(target = Code.Js) ~name ~imports body : Code.jsir_file =
  let body = body ~fresh:Code.Var.fresh in
  let last_var = Code.Var.count () - 1 in
  let block = { Code.params = []; body; branch = Stop } in
  { target
  ; program =
      Code.Marshalable_program.of_program
        { start = 0; blocks = Code.Addr.Map.singleton 0 block; free_pc = 1 }
  ; last_var
  ; var_names = []
  ; imported_compilation_units = imports
  ; exported_compilation_unit = name
  ; force_link = false
  ; symbol_imports = []
  ; symbol_exports = []
  }

let write_jsir_file file (body : Code.jsir_file) =
  let oc = open_out_bin file in
  output_string oc (Magic_number.to_string Magic_number.current_jsir);
  output_value oc body;
  close_out oc

let read_jsir_file file =
  let ic = open_in_bin file in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> Parse_bytecode.from_jsir_channel ~target:Code.Js ~filename:file ic)

let print_unit (unit : Parse_bytecode.jsir_unit) =
  Printf.printf "unit %s\n" unit.name;
  print_string (Unit_info.to_string unit.info);
  let start = unit.program.start in
  Code.Print.block
    Format.std_formatter
    (fun _ _ -> "")
    start
    (Code.Addr.Map.find start unit.program.blocks);
  Format.pp_print_flush Format.std_formatter ()

let unit_a =
  jsir_file ~name:"A" ~imports:[] (fun ~fresh ->
      let greeting = fresh () in
      [ Let (greeting, extern "jsir_test_greet" [ string "A" ])
      ; Let
          ( fresh ()
          , extern
              "caml_register_symbol"
              [ native_string "A"; native_string "greeting"; Pv greeting ] )
      ])

let unit_b =
  jsir_file ~name:"B" ~imports:[ "A" ] (fun ~fresh ->
      let greeting = fresh () in
      [ Let
          ( greeting
          , extern "caml_get_symbol" [ native_string "A"; native_string "greeting" ] )
      ; Let (fresh (), extern "jsir_test_print" [ Pv greeting ])
      ])

let%expect_test "Parse_bytecode.from_jsir_channel" =
  with_temp_dir ~f:(fun () ->
      write_jsir_file "a.cm-jsir-js" unit_a;
      Code.Var.reset ();
      let unit = read_jsir_file "a.cm-jsir-js" in
      print_unit unit;
      (* Fresh variables do not collide with the variables of the unit. *)
      Printf.printf "last_var: %d\n" unit_a.last_var;
      Printf.printf "fresh: %d\n" (Code.Var.idx (Code.Var.fresh ())));
  [%expect
    {|
    unit A
    //# unitInfo: Provides: A
    ==== 0 () ====
      v1 = "jsir_test_greet"("A")
      v2 = "caml_register_symbol"("A"j, "greeting"j, v1)
      stop

    last_var: 2
    fresh: 3
    |}]

let%expect_test
    "Parse_bytecode.from_jsir_channel keeps fresh variables disjoint across units" =
  with_temp_dir ~f:(fun () ->
      write_jsir_file "a.cm-jsir-js" unit_a;
      write_jsir_file "b.cm-jsir-js" unit_b;
      Code.Var.reset ();
      List.iter [ "a.cm-jsir-js"; "b.cm-jsir-js" ] ~f:(fun file ->
          print_unit (read_jsir_file file));
      (* Fresh variables do not collide with the variables of any unit. *)
      Printf.printf "last_var: %d, %d\n" unit_a.last_var unit_b.last_var;
      Printf.printf "fresh: %d\n" (Code.Var.idx (Code.Var.fresh ())));
  [%expect
    {|
    unit A
    //# unitInfo: Provides: A
    ==== 0 () ====
      v1 = "jsir_test_greet"("A")
      v2 = "caml_register_symbol"("A"j, "greeting"j, v1)
      stop

    unit B
    //# unitInfo: Provides: B
    //# unitInfo: Requires: A
    ==== 0 () ====
      v3 = "caml_get_symbol"("A"j, "greeting"j)
      v4 = "jsir_test_print"(v3)
      stop

    last_var: 2, 4
    fresh: 5
    |}]

let stubs =
  {|
//Provides: jsir_test_greet
//Requires: caml_jsstring_of_string, caml_string_of_jsstring
function jsir_test_greet(s) {
  return caml_string_of_jsstring("hello " + caml_jsstring_of_string(s));
}
//Provides: jsir_test_print
//Requires: caml_jsstring_of_string
function jsir_test_print(s) {
  console.log(caml_jsstring_of_string(s));
  return 0;
}
//Provides: jsir_test_unused
function jsir_test_unused() {
  return 0;
}
|}

let setup () =
  Filetype.write_js ~name:"stubs.js" (Filetype.js_text_of_string stubs) |> ignore;
  write_jsir_file "a.cm-jsir-js" unit_a;
  write_jsir_file "b.cm-jsir-js" unit_b;
  js_of_ocaml [ "build-runtime"; "-o"; "runtime.js" ] |> print_string

(* [filter] selects the lines of the program's output to print. *)
let link_and_run ?(filter = fun _ -> true) files =
  js_of_ocaml ([ "link"; "runtime.js" ] @ files @ [ "-o"; "bin.js" ]) |> print_string;
  run_javascript (Filetype.js_file_of_path "bin.js")
  |> String.split_on_char ~sep:'\n'
  |> List.iter ~f:(fun line -> if filter line then print_endline line)

(* The unit info headers of a generated file. *)
let print_unit_infos file =
  List.iter (file_lines_text file) ~f:(fun line ->
      if String.starts_with ~prefix:Unit_info.prefix line then print_endline line)

let%expect_test "JSIR file without --include-runtime" =
  with_temp_dir ~f:(fun () ->
      setup ();
      js_of_ocaml [ "stubs.js"; "a.cm-jsir-js"; "-o"; "a.js" ] |> print_string;
      js_of_ocaml [ "b.cm-jsir-js"; "-o"; "b.js" ] |> print_string;
      print_unit_infos "a.js";
      print_unit_infos "b.js";
      (* The stubs are not part of the output, so the program fails. Only the
         error itself is stable (the rest is a stack trace through a temporary
         directory). *)
      link_and_run ~filter:(String.starts_with ~prefix:"TypeError") [ "a.js"; "b.js" ]);
  [%expect
    {|
    //# unitInfo: Provides: A
    //# unitInfo: Provides: B
    //# unitInfo: Requires: A
    TypeError: b.jsir_test_greet is not a function
    |}]

let%expect_test "JSIR file with --include-runtime" =
  with_temp_dir ~f:(fun () ->
      setup ();
      js_of_ocaml [ "--include-runtime"; "stubs.js"; "a.cm-jsir-js"; "-o"; "a.js" ]
      |> print_string;
      js_of_ocaml [ "b.cm-jsir-js"; "-o"; "b.js" ] |> print_string;
      print_unit_infos "a.js";
      print_unit_infos "b.js";
      link_and_run [ "a.js"; "b.js" ]);
  [%expect
    {|
    //# unitInfo: Provides:
    //# unitInfo: Primitives: jsir_test_greet, jsir_test_print, jsir_test_unused
    //# unitInfo: Force_link: true
    //# unitInfo: Provides: A
    //# unitInfo: Provides: B
    //# unitInfo: Requires: A
    hello A
    |}]

(* The error messages, without the path to the executable. *)
let print_errors output =
  let marker = "Error: " in
  let len = String.length marker in
  String.split_on_char ~sep:'\n' output
  |> List.iter ~f:(fun line ->
      let rec loop i =
        if i + len <= String.length line
        then
          if String.equal (String.sub line ~pos:i ~len) marker
          then print_endline (String.sub line ~pos:i ~len:(String.length line - i))
          else loop (i + 1)
      in
      loop 0)

let%expect_test "js_of_ocaml rejects units compiled for wasm_of_ocaml" =
  with_temp_dir ~f:(fun () ->
      let unit = jsir_file ~target:Code.Wasm ~name:"W" ~imports:[] (fun ~fresh:_ -> []) in
      write_jsir_file "w.cm-jsir-wasm" unit;
      write_jsir_file "w.cm-jsir-js" unit;
      print_errors (js_of_ocaml ~fail:false [ "w.cm-jsir-wasm"; "-o"; "w.js" ]);
      print_errors (js_of_ocaml ~fail:false [ "w.cm-jsir-js"; "-o"; "w.js" ]));
  [%expect
    {|
    Error: w.cm-jsir-wasm was compiled for wasm_of_ocaml (ocamlopt -target wasm_of_ocaml) and cannot be compiled to JavaScript
    Error: w.cm-jsir-js was compiled for wasm_of_ocaml (ocamlopt -target wasm_of_ocaml) and cannot be compiled to JavaScript
    |}]
