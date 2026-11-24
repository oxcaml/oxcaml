(* Js_of_ocaml tests
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

(* Tests for the [Tail_call] pass of the Wasm backend. *)

module Code = Js_of_ocaml_compiler.Code
module W = Wasm_of_ocaml_compiler.Wasm_ast
module Tail_call = Wasm_of_ocaml_compiler.Tail_call

let%expect_test "a call followed by a return of its result becomes a tail call" =
  let f = Code.Var.fresh () in
  let x = Code.Var.fresh () in
  let l = [ W.LocalSet (x, W.Call (f, [])); W.Return (Some (W.LocalGet x)) ] in
  (match Tail_call.f ~no_tail_call:(Code.Var.Hashtbl.create 1) l with
  | [ W.Return_call (g, []) ] ->
      Printf.printf "return_call of the called function: %b\n" (Code.Var.equal g f)
  | _ -> Printf.printf "not rewritten\n");
  [%expect {| return_call of the called function: true |}]

let%expect_test "a flat list of many instructions does not overflow the stack" =
  (* The body of a function can be a flat list of hundreds of thousands of
     instructions (e.g. the initialization code of a whole program); a stack
     frame per instruction would overflow the default 8 MiB stack. *)
  let x = Code.Var.fresh () in
  let n = 1_000_000 in
  let l = ref [ W.Return (Some (W.LocalGet x)) ] in
  for _ = 1 to n do
    l := W.Drop (W.LocalGet x) :: !l
  done;
  let l' = Tail_call.f ~no_tail_call:(Code.Var.Hashtbl.create 1) !l in
  Printf.printf "lengths agree: %b\n" (List.length l' = n + 1);
  [%expect {| lengths agree: true |}]
