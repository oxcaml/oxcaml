(* TEST
 flambda2;
 include stdlib_stable;
 include stdlib_upstream_compatible;
 {
   bytecode;
 }{
   native;
 }{
   jsir;
 }
*)

(* Block index accesses into flat float blocks (records of [float#]s and arrays
   of [float#]s), and into the [float#] field of a mixed record. *)

open Stdlib_stable
open Stdlib_upstream_compatible

external[@layout_poly] makearray_dynamic :
  ('a : any mod separable). int -> 'a -> 'a array =
  "%makearray_dynamic"

external[@layout_poly] aget : ('a : any mod separable). 'a array -> int -> 'a =
  "%array_safe_get"

external[@layout_poly] aset :
  ('a : any mod separable). 'a array -> int -> 'a -> unit = "%array_safe_set"

type t = { mutable x : float#; mutable y : float# }

let[@inline never] get_f (r : t) idx : float# = Idx_mut.get r idx

let[@inline never] set_f (r : t) idx (v : float#) = Idx_mut.set r idx v

let print_record label r =
  Printf.printf "%s: %.1f %.1f\n" label
    (Float_u.to_float (get_f r (.x)))
    (Float_u.to_float (get_f r (.y)))

let () =
  print_endline "All-float# record";
  let r = { x = #1.0; y = #2.0 } in
  print_record "allocate, read using idx" r;
  set_f r (.x) #3.0;
  set_f r (.y) #4.0;
  Printf.printf "write using idx, read using record: %.1f %.1f\n"
    (Float_u.to_float r.x) (Float_u.to_float r.y);
  r.x <- #5.0;
  r.y <- #6.0;
  print_record "write using record, read using idx" r;
  set_f r (.x) #7.0;
  set_f r (.y) #8.0;
  print_record "write using idx, read using idx" r

let[@inline never] get_elt (a : float# array) idx : float# =
  Idx_mut.get a idx

let[@inline never] set_elt (a : float# array) idx (v : float#) =
  Idx_mut.set a idx v

let idx i = Idx_mut.unsafe_create_into_array i

let print_array label read =
  Printf.printf "%s: %.1f %.1f %.1f\n" label (read 0) (read 1) (read 2)

let read_idx a i = Float_u.to_float (get_elt a (idx i))

let read_array a i = Float_u.to_float (aget a i)

let () =
  print_endline "float# array";
  let a = [| #1.0; #2.0; #3.0 |] in
  print_array "allocate literal, read using idx" (read_idx a);
  let a = makearray_dynamic 3 #4.0 in
  print_array "allocate dynamically, read using idx" (read_idx a);
  for i = 0 to 2 do
    set_elt a (idx i) (Float_u.of_float (Float.of_int (i + 5)))
  done;
  print_array "write using idx, read using array" (read_array a);
  for i = 0 to 2 do
    aset a i (Float_u.of_float (Float.of_int (i + 8)))
  done;
  print_array "write using array, read using idx" (read_idx a);
  for i = 0 to 2 do
    set_elt a (idx i) (Float_u.of_float (Float.of_int (i + 11)))
  done;
  print_array "write using idx, read using idx" (read_idx a)

type m = { s : string; mutable u : float# }

let[@inline never] get_u (r : m) idx : float# = Idx_mut.get r idx

let[@inline never] set_u (r : m) idx (v : float#) = Idx_mut.set r idx v

let () =
  print_endline "Mixed record";
  let r = { s = "foo"; u = #1.0 } in
  Printf.printf "%s %.1f\n" r.s (Float_u.to_float (get_u r (.u)));
  set_u r (.u) #2.0;
  Printf.printf "%s %.1f\n" r.s (Float_u.to_float r.u)
