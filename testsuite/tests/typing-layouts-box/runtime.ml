(* TEST
 include stdlib_upstream_compatible;
 flambda2;
 {
   native;
 } {
   bytecode;
 }
*)

(* This tests that values at box kinds act the same at runtime. A box kind
   cannot yet be inhabited organically, so we use magic. *)

module Boxed : sig
  type f : float64 box (* inhabited by [float] *)

  type i : untagged_immediate box (* inhabited by [int] *)

  val of_float : float -> f

  val to_float : f -> float

  val of_int : int -> i

  val to_int : i -> int
end = struct
  (* [Obj.magic] applies as box kinds are subkinds of [value] *)
  type f : float64 box

  type i : untagged_immediate box

  let of_float (x : float) : f = Obj.magic x

  let to_float (x : f) : float = Obj.magic x

  let of_int (x : int) : i = Obj.magic x

  let to_int (x : i) : int = Obj.magic x
end

let checkf (x : Boxed.f) v = assert (Float.equal (Boxed.to_float x) v)

let checki (x : Boxed.i) v = assert (Int.equal (Boxed.to_int x) v)

(* Round-trips *)
let () = checkf (Boxed.of_float 3.25) 3.25

let () = checki (Boxed.of_int 3) 3

(* Through a function call *)
let[@inline never] through (x : Boxed.f) = x

let () = checkf (through (Boxed.of_float 4.5)) 4.5

(* As record fields at box kinds *)
type ('a : float64 box) r = { fb : 'a; s : string }

let () =
  let r = { fb = Boxed.of_float 5.75; s = "s" } in
  checkf r.fb 5.75;
  assert (String.equal r.s "s")

(* Including in a mixed block *)
module Float_u = Stdlib_upstream_compatible.Float_u

type ('a : untagged_immediate box) mixed = { ib : 'a; fl : float# }

let () =
  let m = { ib = Boxed.of_int 6; fl = #7.5 } in
  checki m.ib 6;
  assert (Float.equal (Float_u.to_float m.fl) 7.5)

(* As a variant field *)
type v = V of Boxed.f * string

let () =
  match V (Boxed.of_float 8.5, "v") with
  | V (x, s) ->
    checkf x 8.5;
    assert (String.equal s "v")

(* As an unboxed record field expecting [any box] *)
type ('a : any box) ur = #{ a : 'a; s : string }

let () =
  let u = #{ a = Boxed.of_float 9.25; s = "u" } in
  checkf u.#a 9.25;
  assert (String.equal u.#s "u")

(* In unboxed tuples: one whose components are all gc-scannable, and one whose
   components are all gc-ignorable *)
let () =
  let #(a, s) = #(Boxed.of_float 10.5, "b") in
  checkf a 10.5;
  assert (String.equal s "b")

let () =
  let #(a, b) = #(Boxed.of_int 11, 12) in
  checki a 11;
  assert (Int.equal b 12)
