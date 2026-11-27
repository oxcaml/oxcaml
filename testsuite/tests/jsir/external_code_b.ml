(* Paired with external_code.ml: a unit that creates closures from the code of
   External_code_a, and whose functions other units inline in turn. *)

let[@inline] make_adder n = External_code_a.make_adder (n + 1)

let[@inline] make_first n = External_code_a.make_first (n + 1)

let add3 = External_code_a.make_adder (Sys.opaque_identity 3)

module Int_max = External_code_a.Make (Int)

module Opaque_int : External_code_a.S with type t = int =
  (val Sys.opaque_identity (module Int : External_code_a.S with type t = int))

module Opaque_max = External_code_a.Make (Opaque_int)
