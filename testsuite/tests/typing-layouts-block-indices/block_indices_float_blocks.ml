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

type t = { mutable x : float#; mutable y : float# }

let[@inline never] get_f (r : t) idx : float# = Idx_mut.get r idx

let[@inline never] set_f (r : t) idx (v : float#) = Idx_mut.set r idx v

let () =
  print_endline "All-float# record";
  let r = { x = #1.0; y = #2.0 } in
  Printf.printf "%.1f %.1f\n"
    (Float_u.to_float (get_f r (.x)))
    (Float_u.to_float (get_f r (.y)));
  set_f r (.x) #3.0;
  set_f r (.y) #4.0;
  Printf.printf "%.1f %.1f\n" (Float_u.to_float r.x) (Float_u.to_float r.y)

let[@inline never] get_elt (a : float# array) idx : float# =
  Idx_mut.get a idx

let[@inline never] set_elt (a : float# array) idx (v : float#) =
  Idx_mut.set a idx v

let () =
  print_endline "float# array";
  let a = makearray_dynamic 3 #0.0 in
  for i = 0 to 2 do
    set_elt a (Idx_mut.unsafe_create_into_array i)
      (Float_u.of_float (Float.of_int (i + 1)))
  done;
  for i = 0 to 2 do
    Printf.printf "%.1f\n"
      (Float_u.to_float (get_elt a (Idx_mut.unsafe_create_into_array i)))
  done

type m = { s : string; mutable u : float# }

let[@inline never] get_u (r : m) idx : float# = Idx_mut.get r idx

let[@inline never] set_u (r : m) idx (v : float#) = Idx_mut.set r idx v

let () =
  print_endline "Mixed record";
  let r = { s = "foo"; u = #1.0 } in
  Printf.printf "%s %.1f\n" r.s (Float_u.to_float (get_u r (.u)));
  set_u r (.u) #2.0;
  Printf.printf "%s %.1f\n" r.s (Float_u.to_float r.u)
