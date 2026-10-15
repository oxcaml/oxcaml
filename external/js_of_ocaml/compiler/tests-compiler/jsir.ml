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
  | exception Failure msg -> print_endline msg

let () =
  register "Foo" "x" 42;
  lookup "Foo" "x";
  lookup "Foo" "y";
  lookup "Bar" "x"
|};
  [%expect
    {|
    Foo.x = 42
    Error: caml_get_symbol: compilation unit Foo does not define symbol y
    Error: caml_get_symbol: compilation unit Bar has not been initialised yet (looking up x)
    |}]

(* Synthetic [.cmj] and [.cmja] files, laid out as [ocamlj] and [ocamlj -a]
   produce them. The programs are built directly in JSIR, as the [ocamlj]
   pipeline is not available from these tests. *)

let compilation_unit name =
  Compilation_unit.create
    Compilation_unit.Prefix.empty
    (Compilation_unit.Name.of_string name)

let string s = Code.Pc (Code.String s)

let native_string s = Code.Pc (Code.NativeString (Code.Native_string.of_string s))

let extern name args = Code.Prim (Code.Extern (name, None), args)

(* TODO: we temporarily populate 31 unreachable blocks in addition to the one we want,
   since an [Int_trie] with empty slots cannot be marshalled into [cmj]. *)
let blocks block =
  let empty = { Code.params = []; body = []; branch = Stop } in
  List.fold_left
    (List.init ~len:32 ~f:Fun.id)
    ~init:Code.Addr.Map.empty
    ~f:(fun blocks pc -> Code.Addr.Map.add pc (if pc = 0 then block else empty) blocks)

let cmj_body ~name ~imports body : Code.cmj_body =
  let body = body ~fresh:Code.Var.fresh in
  let last_var = Code.Var.count () - 1 in
  let block = { Code.params = []; body; branch = Stop } in
  { program = { start = 0; blocks = blocks block; free_pc = 32 }
  ; last_var
  ; imported_compilation_units = List.map imports ~f:compilation_unit
  ; exported_compilation_unit = compilation_unit name
  }

let write_cmj file (body : Code.cmj_body) =
  let oc = open_out_bin file in
  output_string oc (Magic_number.to_string Magic_number.current_cmj);
  output_value oc body;
  close_out oc

let write_cmja file bodies =
  let oc = open_out_bin file in
  output_string oc (Magic_number.to_string Magic_number.current_cmja);
  let toc_pos_pos = pos_out oc in
  output_binary_int oc 0;
  let units =
    List.map bodies ~f:(fun (body : Code.cmj_body) ->
        let cu_pos = pos_out oc in
        output_value oc body;
        { Cmj_format.cu_name = body.exported_compilation_unit
        ; cu_pos
        ; cu_codesize = pos_out oc - cu_pos
        })
  in
  let toc_pos = pos_out oc in
  output_value oc { Cmj_format.lib_units = units };
  seek_out oc toc_pos_pos;
  output_binary_int oc toc_pos;
  close_out oc

let read_jsir_file file ~expected f =
  let ic = open_in_bin file in
  let magic = Magic_number.of_string (really_input_string ic Magic_number.size) in
  assert (Magic_number.equal magic (Magic_number.current expected));
  let r = f ic in
  close_in ic;
  r

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
  cmj_body ~name:"A" ~imports:[] (fun ~fresh ->
      let greeting = fresh () in
      [ Let (greeting, extern "jsir_test_greet" [ string "A" ])
      ; Let
          ( fresh ()
          , extern
              "caml_register_symbol"
              [ native_string "A"; native_string "greeting"; Pv greeting ] )
      ])

let unit_b =
  cmj_body ~name:"B" ~imports:[ "A" ] (fun ~fresh ->
      let greeting = fresh () in
      [ Let
          ( greeting
          , extern "caml_get_symbol" [ native_string "A"; native_string "greeting" ] )
      ; Let (fresh (), extern "jsir_test_print" [ Pv greeting ])
      ])

let%expect_test "Parse_bytecode.from_cmj" =
  with_temp_dir ~f:(fun () ->
      write_cmj "a.cmj" unit_a;
      Code.Var.reset ();
      let unit = read_jsir_file "a.cmj" ~expected:`Cmj Parse_bytecode.from_cmj in
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

let%expect_test "Parse_bytecode.from_cmja" =
  with_temp_dir ~f:(fun () ->
      write_cmja "lib.cmja" [ unit_a; unit_b ];
      Code.Var.reset ();
      let units = read_jsir_file "lib.cmja" ~expected:`Cmja Parse_bytecode.from_cmja in
      List.iter units ~f:print_unit;
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
  write_cmj "a.cmj" unit_a;
  write_cmj "b.cmj" unit_b;
  write_cmja "lib.cmja" [ unit_a; unit_b ];
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

let%expect_test "cmj without --include-runtime" =
  with_temp_dir ~f:(fun () ->
      setup ();
      js_of_ocaml [ "stubs.js"; "a.cmj"; "-o"; "a.js" ] |> print_string;
      js_of_ocaml [ "b.cmj"; "-o"; "b.js" ] |> print_string;
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

let%expect_test "cmj with --include-runtime" =
  with_temp_dir ~f:(fun () ->
      setup ();
      js_of_ocaml [ "--include-runtime"; "stubs.js"; "a.cmj"; "-o"; "a.js" ]
      |> print_string;
      js_of_ocaml [ "b.cmj"; "-o"; "b.js" ] |> print_string;
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

let%expect_test "cmja with --include-runtime" =
  with_temp_dir ~f:(fun () ->
      setup ();
      js_of_ocaml [ "--include-runtime"; "stubs.js"; "lib.cmja"; "-o"; "lib.js" ]
      |> print_string;
      print_unit_infos "lib.js";
      link_and_run [ "lib.js" ]);
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

let%expect_test "cmja with --include-runtime and --keep-unit-names" =
  with_temp_dir ~f:(fun () ->
      setup ();
      Unix.mkdir "out" 0o755;
      js_of_ocaml
        [ "--include-runtime"; "--keep-unit-names"; "stubs.js"; "lib.cmja"; "-o"; "out/" ]
      |> print_string;
      List.iter
        (List.sort ~cmp:String.compare (Array.to_list (Sys.readdir "out")))
        ~f:(fun f ->
          let file = Filename.concat "out" f in
          Printf.printf "$ %s\n" file;
          print_unit_infos file);
      link_and_run [ "out/runtime.js"; "out/A.js"; "out/B.js" ]);
  [%expect
    {|
    $ out/A.js
    //# unitInfo: Provides: A
    $ out/B.js
    //# unitInfo: Provides: B
    //# unitInfo: Requires: A
    $ out/runtime.js
    //# unitInfo: Provides:
    //# unitInfo: Primitives: jsir_test_greet, jsir_test_print, jsir_test_unused
    //# unitInfo: Force_link: true
    hello A
    |}]
