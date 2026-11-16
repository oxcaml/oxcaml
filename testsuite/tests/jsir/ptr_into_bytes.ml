(* TEST
 flambda2;
 include stdlib_stable;
 include stdlib_upstream_compatible;
 jsir;
*)

(* Pointers to bytes whose base is a string, bytes or bigarray instead of a
   block. Block indices count 8 bytes for every field under JSIR, so the index
   of the byte at position [i] is [8 * i]. *)

open Stdlib_stable
open Stdlib_upstream_compatible

external idx : 'a ('b : any). int64_u -> ('a, 'b) idx_mut = "%identity"

external[@layout_poly] get :
  'a ('b : any). #('a * ('a, 'b) idx_mut) -> 'b = "%unsafe_get_ptr"

external[@layout_poly] set :
  'a ('b : any). #('a * ('a, 'b) idx_mut) -> 'b -> unit = "%unsafe_set_ptr"

let[@inline never] byte_idx : 'a ('b : any). int -> ('a, 'b) idx_mut =
 fun i -> idx (Int64_u.of_int (8 * i))

let[@inline never] get_byte base i = Int8_u.to_int (get #(base, byte_idx i))

let[@inline never] set_byte base i v =
  set #(base, byte_idx i) (Int8_u.of_int v)

let[@inline never] get_two_bytes base i =
  let #(x, y) : #(int8# * int8#) = get #(base, byte_idx i) in
  Int8_u.to_int x, Int8_u.to_int y

let print_out_of_bounds f =
  match f () with
  | (_ : int) -> print_endline "no exception"
  | exception Invalid_argument msg -> Printf.printf "Invalid_argument %S\n" msg

let () =
  let s = "abc\255" in
  Printf.printf "string: %d %d %d %d\n" (get_byte s 0) (get_byte s 1)
    (get_byte s 2) (get_byte s 3);
  let x, y = get_two_bytes s 1 in
  Printf.printf "string, two bytes: %d %d\n" x y;
  print_out_of_bounds (fun () -> get_byte s 4)

let () =
  let b = Bytes.of_string "abc" in
  Printf.printf "bytes: %d %d %d\n" (get_byte b 0) (get_byte b 1)
    (get_byte b 2);
  set_byte b 0 (Char.code 'x');
  set_byte b 2 (-1);
  Printf.printf "bytes after writes: %S %d\n" (Bytes.to_string b) (get_byte b 2);
  print_out_of_bounds (fun () -> get_byte b 3);
  print_out_of_bounds (fun () -> set_byte b 3 0; 0)

let () =
  let ba = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 3 in
  Bigarray.Array1.set ba 0 'a';
  Bigarray.Array1.set ba 1 'b';
  Bigarray.Array1.set ba 2 '\255';
  Printf.printf "bigarray: %d %d %d\n" (get_byte ba 0) (get_byte ba 1)
    (get_byte ba 2);
  set_byte ba 1 (Char.code 'y');
  set_byte ba 2 (-128);
  Printf.printf "bigarray after writes: %C %d\n" (Bigarray.Array1.get ba 1)
    (Char.code (Bigarray.Array1.get ba 2));
  print_out_of_bounds (fun () -> get_byte ba 3);
  print_out_of_bounds (fun () -> set_byte ba 3 0; 0)

(* Bytes in blocks are still read and written through pointers. *)

type record = { mutable x : int8#; s : string; mutable y : int8# }

let () =
  let r = { x = #1s; s = "s"; y = -#2s } in
  Printf.printf "record: %d %d\n"
    (Int8_u.to_int (get #(r, (.x))))
    (Int8_u.to_int (get #(r, (.y))));
  set #(r, (.x)) #3s;
  set #(r, (.y)) (-#4s);
  Printf.printf "record after writes: %d %s %d\n" (Int8_u.to_int r.x) r.s
    (Int8_u.to_int r.y)
