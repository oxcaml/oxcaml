(* TEST
 flambda;
 ocamlopt_flags = "-flambda2-expert-cmm-safe-subst -flambda2-expert-cmm-pin-local-values";
 native;
*)

(* The translation to Cmm must not move an expression that inspects a value
   allocated in a region past the end of that region. *)

(* Issue #8080: the read of [q] is used only in a continuation with two uses,
   whose handler runs after [exclave_] has ended the region of [q]. *)

let[@inline never] mk (a : int) (b : int) = exclave_ stack_ (a, b)

let[@inline never] exclave_both_arms a b c =
  let q = mk a b in
  let x = fst q in
  let finish y =
    let p = Sys.opaque_identity (stack_ (y + 1000, y + 2000)) in
    let u, v = p in
    x + u + v - 3000 - (2 * y)
  in
  if c then exclave_ finish 1 else exclave_ finish 2

(* As above, but the local value reaches the read through a continuation
   parameter (the join point of the [if]). *)

let[@inline never] join (a : int) (b : int) c d =
  let q =
    if c
    then Sys.opaque_identity (stack_ (a, b))
    else Sys.opaque_identity (stack_ (b, a))
  in
  let x = fst q in
  let finish y =
    let p = Sys.opaque_identity (stack_ (y + 1000, y + 2000)) in
    let u, v = p in
    x + u + v - 3000 - (2 * y)
  in
  if d then exclave_ finish 1 else exclave_ finish 2

(* Issue #8114: the read of [q] is used in the handler of the [try], after the
   region of the [try] has ended. *)

exception E of int

let[@inline never] try_region a b =
  try
    let q = Sys.opaque_identity (stack_ (a, b)) in
    let x = fst q in
    if b > 0 then raise (E x) else 0
  with E y ->
    let p = Sys.opaque_identity (stack_ (b + 1000, b + 2000)) in
    let u, v = p in
    y + u + v - 3000 - (2 * b)

(* [Is_int] of a local value does not read memory, but moving it past the end
   of the region keeps the pointer live across the allocations that follow,
   where the memory has been reused. A GC there takes the word before the
   pointer for a block header and changes its colour. *)

type fr =
  { mutable a : float;
    mutable b : float;
    mutable c : float
  }

let[@inline never] mk_opt n =
  exclave_ (if n > 0 then stack_ (Some n) else None)

let[@inline never] check (local_ r) =
  Int64.equal (Int64.bits_of_float r.b) 0x300L

let[@inline never] is_int n c k =
  let q = mk_opt n in
  let b = match q with None -> true | Some _ -> false in
  let finish y =
    let r = stack_ { a = k; b = k; c = k } in
    let h = ref y in
    let res = b, h in
    let ok = check r in
    ignore (Sys.opaque_identity res);
    ok
  in
  if c then exclave_ finish 1 else exclave_ finish 2

let () =
  Printf.printf "exclave_both_arms: %d\n"
    (exclave_both_arms 10 20 (Sys.opaque_identity true));
  Printf.printf "join: %d\n"
    (join 10 20 (Sys.opaque_identity true) (Sys.opaque_identity true));
  Printf.printf "try_region: %d\n" (try_region 10 20);
  let k = Int64.float_of_bits 0x300L in
  let corrupted = ref 0 in
  for i = 1 to 1_000_000 do
    if not (is_int i (Sys.opaque_identity true) k) then incr corrupted
  done;
  Printf.printf "is_int: %s\n" (if !corrupted = 0 then "ok" else "corrupted")
