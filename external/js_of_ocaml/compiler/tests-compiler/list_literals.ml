(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

(* A list whose cells are not compile-time constants (non-constant elements or a
   non-constant tail, e.g. [ppx_enumerate]'s [all] for a variant with an
   [[@enumerate.custom]] constructor) is a chain of [Block]s that the expression
   queue inlines into one nested array literal [[0,a,[0,b,...]]]. A few hundred
   levels overflow the stack of some JavaScript parsers. Past [list_depth] cells,
   the heads are emitted as a flat array that is converted at runtime instead. *)

open Util

let n = 1000

let prog =
  Printf.sprintf
    {|
let with_tail tl = %s :: tl
let non_constant_elements x = [ %s ]
let short x = [ x; x + 1; x + 2 ]
let effects f = [ %s ]
|}
    (String.concat " :: " (List.init n string_of_int))
    (String.concat "; " (List.init n (Printf.sprintf "x + %d")))
    (String.concat "; " (List.init 200 (Printf.sprintf "f %d")))

(* Maximal nesting of array literals, and the code with long runs of list
   elements elided. *)
let summarize program name =
  let module J = Js_of_ocaml_compiler.Javascript in
  let s = expression_to_string (J.EFun (None, find_function program name)) in
  let depth = ref 0 and max_depth = ref 0 in
  String.iter
    (function
      | '[' ->
          incr depth;
          max_depth := max !max_depth !depth
      | ']' -> decr depth
      | _ -> ())
    s;
  let elided =
    s
    |> Str.global_replace (Str.regexp "\\([^,]+,\\)\\([^,]+,\\)\\([^,]+,\\)+") "\\1...,"
    |> Str.global_replace (Str.regexp "\\]\\]\\]+") "]...]"
  in
  Printf.printf "max nesting: %d\n%s" !max_depth elided

(* In these pretty-printed outputs, the local alias of
   [caml_list_of_js_array_with_tail] is shown truncated to 30 characters, as are
   all variable names. *)
let%expect_test "long lists are built from flat arrays" =
  let program = compile_and_parse prog in
  summarize program "with_tail";
  [%expect
    {|
    max nesting: 1
    (function(tl){
      return caml_list_of_js_array_with_tai
              ([0,...,
               tl);
     });
    //end
    |}];
  summarize program "non_constant_elements";
  [%expect
    {|
    max nesting: 1
    (function(x){
      return caml_list_of_js_array
              ([x | 0,...,
                x + 999 | 0]);
     });
    //end
    |}];
  print_fun_decl program (Some "short");
  [%expect
    {|
    function short(x){return [0, x, [0, x + 1 | 0, [0, x + 2 | 0, 0]]];}
    //end
    |}]

let%expect_test "without the rewrite, long lists are deeply nested literals" =
  let program = compile_and_parse ~flags:[ "--set=list_depth=100000" ] prog in
  summarize program "with_tail";
  [%expect
    {|
    max nesting: 1000
    (function(tl){
      return [0,..., tl]...];
     });
    //end
    |}]

let%expect_test "only lists longer than list_depth are rewritten" =
  let prog len =
    Printf.sprintf "let f tl = %s :: tl" (String.concat " :: " (List.init len string_of_int))
  in
  let flags = [ "--set=list_depth=3" ] in
  print_fun_decl (compile_and_parse ~flags (prog 3)) (Some "f");
  [%expect
    {|
    function f(tl){return [0, 0, [0, 1, [0, 2, tl]]];}
    //end
    |}];
  print_fun_decl (compile_and_parse ~flags (prog 4)) (Some "f");
  [%expect
    {|
    function f(tl){return caml_list_of_js_array_with_tai([0, 1, 2, 3], tl);}
    //end
    |}]

let%expect_test "calls keep their evaluation order" =
  (* Each call flushes the expression queue, so the cell built just before it is
     bound to a variable and no chain forms: the output is unchanged, and the
     calls are made right to left, as in OCaml. *)
  let prog = "let effects f = [ f 0; f 1; f 2; f 3; f 4 ]" in
  print_fun_decl (compile_and_parse ~flags:[ "--set=list_depth=3" ] prog) (Some "effects");
  [%expect
    {|
    function effects(f){
     var
      _a_ = [0, caml_call1(f, 4), 0],
      _a_ = [0, caml_call1(f, 3), _a_],
      _a_ = [0, caml_call1(f, 2), _a_],
      _a_ = [0, caml_call1(f, 1), _a_];
     return [0, caml_call1(f, 0), _a_];
    }
    //end
    |}]

let%expect_test "long lists have the expected values" =
  compile_and_run
    (prog
    ^ {|
let check name b = Printf.printf "%s: %b\n" name b
let rec drop k l = if k = 0 then l else drop (k - 1) (List.tl l)
let () =
  let tail = [ -1; -2 ] in
  let l = with_tail tail in
  check "with_tail" (l = List.init 1000 (fun i -> i) @ tail);
  check "tail is shared" (drop 1000 l == tail);
  check "non_constant_elements" (non_constant_elements 7 = List.init 1000 (fun i -> 7 + i));
  check "fresh" (non_constant_elements 7 != non_constant_elements 7);
  check "short" (short 7 = [ 7; 8; 9 ]);
  (* Mutating one cell of a rewritten list only affects that cell. *)
  let l = non_constant_elements 0 in
  Obj.set_field (Obj.repr (drop 500 l)) 0 (Obj.repr (-500));
  check "mutated" (l = List.init 1000 (fun i -> if i = 500 then -500 else i));
  check "other lists unaffected" (non_constant_elements 0 = List.init 1000 (fun i -> i));
  let calls = ref [] in
  let l = effects (fun i -> calls := i :: !calls; i) in
  check "effects" (l = List.init 200 (fun i -> i));
  (* OCaml evaluates the elements from right to left. *)
  check "effects order" (!calls = List.init 200 (fun i -> i))
|});
  [%expect
    {|
    with_tail: true
    tail is shared: true
    non_constant_elements: true
    fresh: true
    short: true
    mutated: true
    other lists unaffected: true
    effects: true
    effects order: true
    |}]
