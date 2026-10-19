(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Hugo Heuzard
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

open Util

let%expect_test "inline recursive function" =
  let program = compile_and_parse {|
    let rec f () = f ()
    and g () = f ()
  |} in
  print_fun_decl program (Some "f");
  print_fun_decl program (Some "g");
  [%expect
    {|
    function f(param){for(;;) ;}
    //end
    function g(param){for(;;) ;}
    //end |}]

let%expect_test "inline small function exposing more tc" =
  let program =
    compile_and_parse
      {|
    let ( >>= ) x f = match x with `Ok v -> f v | `Error _ as e -> e

    let f g x =
      x >>= fun x ->
      g x >>= fun y ->
      y
  |}
  in
  print_fun_decl program (Some "f");
  print_fun_decl program (Some "g");
  [%expect
    {|
    function f(g, x){
     var variant = x[1];
     if(106380200 <= variant) return x;
     var v = x[2], x$0 = caml_call1(g, v), variant$0 = x$0[1];
     if(106380200 <= variant$0) return x$0;
     var v$0 = x$0[2];
     return v$0;
    }
    //end
    not found
    |}]

(* When inline_recursively inlines a function passed as argument,
   the actual argument is still referenced in block arguments.
   Without forced duplication, the closure's params would conflict
   with the intermediate block's params. *)
let%expect_test "inline_recursively must duplicate closure" =
  let program =
    compile_and_parse
      ~flags:[ "--debug"; "invariant" ]
      {|
    let hash_fold_int acc x = 7 * acc + x
    let as_int f s x = hash_fold_int s (f x)
    let hash_fold_char = as_int Char.code
    let hash_char x = hash_fold_char 0 x
  |}
  in
  print_fun_decl program (Some "hash_char");
  [%expect {|
    function hash_char(x){return hash_fold_int(0, x);}
    //end
    |}]

(* Inlining [some] into [apply_some] must not make [call_some] look like its
   last use. *)
let%expect_test "function inlined both as an argument and directly" =
  (try
     compile_and_run
       ~flags:[ "--debug"; "invariant" ]
       {|
    let some x = Some x
    let apply f x = f x
    let apply_some x = apply some x
    let call_some x = some x
    let () =
      List.iter (fun x -> print_int (Option.get x)) [ apply_some 0; apply_some 1; call_some 2 ]
  |}
   with Failure e -> print_endline e);
  [%expect {| 012 |}]

let%expect_test "inlining nested continuations keeps the code size linear" =
  let open Js_of_ocaml_compiler in
  Config.set_target `JavaScript;
  Config.set_effects_backend `Disabled;
  let nested_binds depth =
    let vars = List.init depth (Printf.sprintf "x%d") in
    Printf.sprintf
      "let bind m f = f m\nlet x = %s%s%s"
      (String.concat "" (List.map (Printf.sprintf "bind 0 (fun %s -> ") vars))
      (String.concat " + " vars)
      (String.make depth ')')
  in
  let blocks_after_inlining depth =
    with_temp_dir ~f:(fun () ->
        let cmo =
          Filetype.ocaml_text_of_string (nested_binds depth)
          |> Filetype.write_ocaml ~name:"test.ml"
          |> compile_ocaml_to_cmo
        in
        let ic = open_in_bin (Filetype.path_of_cmo_file cmo) in
        let p =
          match Parse_bytecode.from_channel ic with
          | `Cmo unit -> (Parse_bytecode.from_cmo unit ic).code
          | _ -> assert false
        in
        close_in ic;
        (* Mark calls as exact, as the driver does before inlining *)
        let p, info = Flow.f p in
        let shape, set_shape =
          Flow.the_shape_of
            ~return_values:(Code.return_values p)
            ~pure:Pure_fun.empty
            ~blocks:false
            info
        in
        let p =
          Specialize.f ~shape ~set_shape ~update_def:(Flow.Info.update_def info) p
        in
        let p, live_vars = Deadcode.f (Pure_fun.f p) p in
        let p = Inline.f ~profile:Profile.O3 p live_vars in
        Code.Addr.Map.fold (fun _ _ n -> n + 1) p.blocks 0)
  in
  List.iter
    (fun depth -> Printf.printf "depth %d: %d blocks\n" depth (blocks_after_inlining depth))
    [ 1; 2; 3; 4; 5; 6 ];
  [%expect
    {|
    depth 1: 8 blocks
    depth 2: 12 blocks
    depth 3: 16 blocks
    depth 4: 20 blocks
    depth 5: 24 blocks
    depth 6: 28 blocks
    |}]
