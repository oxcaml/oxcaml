(* TEST
 include stdlib_upstream_compatible;
 flambda2;
 flags = "-extension layouts_beta";
 {
   native;
 } {
   bytecode;
 }
*)
(* CR-soon rtjoa: remove layouts_beta once records-containing-any is stable *)

(* This just tests that values at [addressable] kinds still act the same at
   runtime. Not much interesting is going on here yet, as addressability does
   not (yet) affect boxed representations. *)

module Float_u = Stdlib_upstream_compatible.Float_u

(* We cannot inhabit a made-addressable kind organically, but we can via
   magic; addressability does not change the runtime representation. *)
module F64a : sig
  type t : float64 addressable

  val of_float_u : float# -> t

  val to_float_u : t -> float#
end = struct
  type t : float64 addressable

  external magic_to : ('a : any) ('b : any addressable). 'a -> 'b
    = "%identity"
    [@@layout_poly]

  external magic_of : ('a : any addressable) ('b : any). 'a -> 'b
    = "%identity"
    [@@layout_poly]

  let of_float_u (x : float#) : t = magic_to x

  let to_float_u (x : t) : float# = magic_of x
end

let check (x : F64a.t) v = assert (Float_u.to_float (F64a.to_float_u x) = v)

(* Round-trip *)
let () = check (F64a.of_float_u #3.25) 3.25

(* Through a function call *)
let[@inline never] through (x : F64a.t) = x

let () = check (through (F64a.of_float_u #4.5)) 4.5

(* As a record field at kind [float64 addressable] *)
type ('a : float64 addressable) r = { fa : 'a; s : string }

let () =
  let r = { fa = F64a.of_float_u #5.75; s = "s" } in
  check r.fa 5.75;
  assert (String.equal r.s "s")

(* As a variant field *)
type v = V of F64a.t * string

let () =
  match V (F64a.of_float_u #6.5, "v") with
  | V (x, s) ->
    check x 6.5;
    assert (String.equal s "v")

(* As an unboxed record field expecting [any], then [any addressable] *)
type ('a : any) ur_any = #{ a : 'a; s : string }

let () =
  let u : F64a.t ur_any = #{ a = F64a.of_float_u #7.25; s = "u" } in
  check u.#a 7.25;
  assert (String.equal u.#s "u")

type ('a : any addressable) ur_addr = #{ a : 'a; s : string }

let () =
  let u : F64a.t ur_addr = #{ a = F64a.of_float_u #8.25; s = "u" } in
  check u.#a 8.25;
  assert (String.equal u.#s "u")

(* In unboxed tuples with one, then two, made-addressable components *)
let () =
  let #(a, s) = #(F64a.of_float_u #9.5, "b") in
  check a 9.5;
  assert (String.equal s "b")

let () =
  let #(a, b) = #(F64a.of_float_u #10.5, F64a.of_float_u #11.5) in
  check a 10.5;
  check b 11.5
