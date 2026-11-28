(* TEST
 flambda2;
 include stdlib_stable;
 include stdlib_upstream_compatible;
 jsir;
*)

(* Pointers to bytes and block indices of bytes whose base is a string, bytes or
   bigarray instead of a block. Block indices count 8 bytes for every field
   under JSIR, so the index of the byte at position [i] is [8 * i]. *)

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

external[@layout_poly] get_idx :
  'a ('b : any). 'a -> ('a, 'b) idx_mut -> 'b = "%get_idx"

external[@layout_poly] set_idx :
  'a ('b : any). 'a -> ('a, 'b) idx_mut -> 'b -> unit = "%set_idx"

let[@inline never] get_byte_idx base i =
  Int8_u.to_int (get_idx base (byte_idx i))

let[@inline never] set_byte_idx base i v =
  set_idx base (byte_idx i) (Int8_u.of_int v)

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

(* The same accesses through block indices rather than pointers. *)

let () =
  let s = "abc\255" in
  Printf.printf "string, idx: %d %d %d %d\n" (get_byte_idx s 0)
    (get_byte_idx s 1) (get_byte_idx s 2) (get_byte_idx s 3);
  print_out_of_bounds (fun () -> get_byte_idx s 4)

let () =
  let b = Bytes.of_string "abc" in
  Printf.printf "bytes, idx: %d %d %d\n" (get_byte_idx b 0) (get_byte_idx b 1)
    (get_byte_idx b 2);
  set_byte_idx b 0 (Char.code 'x');
  set_byte_idx b 2 (-1);
  Printf.printf "bytes after idx writes: %S %d\n" (Bytes.to_string b)
    (get_byte_idx b 2);
  print_out_of_bounds (fun () -> get_byte_idx b 3);
  print_out_of_bounds (fun () -> set_byte_idx b 3 0; 0)

let () =
  let ba = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 3 in
  Bigarray.Array1.set ba 0 'a';
  Bigarray.Array1.set ba 1 'b';
  Bigarray.Array1.set ba 2 '\255';
  Printf.printf "bigarray, idx: %d %d %d\n" (get_byte_idx ba 0)
    (get_byte_idx ba 1) (get_byte_idx ba 2);
  set_byte_idx ba 1 (Char.code 'y');
  set_byte_idx ba 2 (-128);
  Printf.printf "bigarray after idx writes: %C %d\n" (Bigarray.Array1.get ba 1)
    (Char.code (Bigarray.Array1.get ba 2));
  print_out_of_bounds (fun () -> get_byte_idx ba 3);
  print_out_of_bounds (fun () -> set_byte_idx ba 3 0; 0)

let () =
  let r = { x = #1s; s = "s"; y = -#2s } in
  Printf.printf "record, idx: %d %d\n"
    (Int8_u.to_int (get_idx r (.x)))
    (Int8_u.to_int (get_idx r (.y)));
  set_idx r (.x) #3s;
  set_idx r (.y) (-#4s);
  Printf.printf "record after idx writes: %d %s %d\n" (Int8_u.to_int r.x) r.s
    (Int8_u.to_int r.y)

(* An offset that does not fit in 32 bits is out of bounds: it does not wrap
   around to a byte of the base. *)

let[@inline never] get_byte_at base offset =
  Int8_u.to_int (get #(base, idx offset))

let[@inline never] get_byte_at_large_constant base =
  Int8_u.to_int (get #(base, idx #0x1_0000_0008L))

let () =
  let s = "abc" in
  let print_byte f =
    match f () with
    | byte -> Printf.printf "byte: %d\n" byte
    | exception Invalid_argument msg ->
      Printf.printf "Invalid_argument %S\n" msg
  in
  print_byte (fun () -> get_byte_at s #0x1_0000_0008L);
  print_byte (fun () -> get_byte_at_large_constant s)
