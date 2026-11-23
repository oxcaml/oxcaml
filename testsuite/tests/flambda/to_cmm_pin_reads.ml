(* TEST
 flambda;
 ocamlopt_flags = "-flambda2-expert-cmm-safe-subst -flambda2-expert-cmm-pin-reads";
 native;
*)

(* Reads of memory must not be moved by the translation to Cmm past the end
   of the region of the block being read, or past a later write. *)

external float_to_int : (float[@local_opt]) -> int = "%intoffloat"

external iarray_get : ('a iarray[@local_opt]) -> int -> ('a[@local_opt])
  = "%array_unsafe_get"

(* Field of a stack-allocated block, used after [exclave_] in both arms. *)

let[@inline never] mk_pair (a : int) (b : int) = exclave_ stack_ (a, b)

let[@inline never] exclave_block_load a b c =
  let q = mk_pair a b in
  let x = fst q in
  let finish y =
    let p = Sys.opaque_identity (stack_ (y + 1000, y + 2000)) in
    let u, v = p in
    x + u + v - 3000 - (2 * y)
  in
  if c then exclave_ finish 1 else exclave_ finish 2

(* Likewise for the unboxing of a stack-allocated float. *)

let[@inline never] mk_float (a : float) = exclave_ stack_ (a +. 1.)

let[@inline never] exclave_unbox a c =
  let q = mk_float a in
  let x = float_to_int q in
  let finish y =
    let p = Sys.opaque_identity (stack_ (Float.of_int y +. 1000.)) in
    x + float_to_int p - 1000 - y
  in
  if c then exclave_ finish 1 else exclave_ finish 2

(* Likewise for the length of a stack-allocated array. *)

let[@inline never] mk_array (a : int) (b : int) = exclave_ stack_ [| a; b |]

let[@inline never] exclave_array_length a b c =
  let q = mk_array a b in
  let x = Array.length q in
  let finish y =
    let p = Sys.opaque_identity (stack_ (y + 1000, y + 2000, y + 3000)) in
    let u, v, w = p in
    x + u + v + w - 6000 - (3 * y)
  in
  if c then exclave_ finish 1 else exclave_ finish 2

(* Likewise for an element of a stack-allocated immutable array. *)

let[@inline never] mk_iarray (a : int) (b : int) : int iarray =
  exclave_ stack_ [: a; b :]

let[@inline never] exclave_iarray_load a b c =
  let q = mk_iarray a b in
  let x = iarray_get q 0 in
  let finish y =
    let p = Sys.opaque_identity (stack_ (y + 1000, y + 2000)) in
    let u, v = p in
    x + u + v - 3000 - (2 * y)
  in
  if c then exclave_ finish 1 else exclave_ finish 2

(* Field of a stack-allocated block, used in the handler of a [try]. *)

exception E of int

let[@inline never] try_region_block_load a b =
  try
    let q = Sys.opaque_identity (stack_ (a, b)) in
    let x = fst q in
    if b > 0 then raise (E x) else 0
  with E y ->
    let p = Sys.opaque_identity (stack_ (b + 1000, b + 2000)) in
    let u, v = p in
    y + u + v - 3000 - (2 * b)

(* Mutable field read before a call that changes the field and raises. *)

type t = { mutable m : int }

let[@inline never] bump_and_raise q : unit =
  q.m <- q.m + 100;
  raise Exit

let[@inline never] mutable_load_before_raising_call q =
  let r = ref 0 in
  try
    r := q.m;
    bump_and_raise q;
    !r
  with Exit -> !r

let () =
  let c = Sys.opaque_identity true in
  Printf.printf "exclave_block_load: %d\n" (exclave_block_load 10 20 c);
  Printf.printf "exclave_unbox: %d\n" (exclave_unbox 10. c);
  Printf.printf "exclave_array_length: %d\n" (exclave_array_length 10 20 c);
  Printf.printf "exclave_iarray_load: %d\n" (exclave_iarray_load 10 20 c);
  Printf.printf "try_region_block_load: %d\n" (try_region_block_load 10 20);
  Printf.printf "mutable_load_before_raising_call: %d\n"
    (mutable_load_before_raising_call { m = 5 })
