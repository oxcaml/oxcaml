(* TEST_BELOW
(* Blank lines added here to preserve locations. *)









*)

(* from MPR#7624 *)

let[@warning "-32"] f x = x

let g x = x

let h x = x


(* multiple bindings *)

let[@warning "-32"] i x = x
and j x = x

let k x = x
and[@warning "-32"] l x = x

let[@warning "-32"] m x = x
and n x = x

let o x = x
and[@warning "-32"] p x = x


(* recursive bindings *)

let[@warning "-32"] rec q x = x
and r x = x

let[@warning "-32"] rec s x = x
and[@warning "-39"] t x = x

let[@warning "-39"] rec u x = x
and v x = v x


(* disabled then re-enabled warnings *)

module M = struct
  [@@@warning "-32"]
  let f x = x
  let[@warning "+32"] g x = x
  let[@warning "+32"] h x = x
  and i x = x
  let j x = x
  and[@warning "+32"] k x = x
end

(* unused values in functor argument *)
module F (X : sig val x : int end) = struct end

module G (X : sig val x : int end) = X

module H (X : sig val x : int end) = X

module type S = sig
  module F:  sig val x : int end -> sig end
end

(* Nominal type comparison *)

module Nominal = struct
  module type S = sig type t val x : int end

  module F(X:S) = struct type t = X.t end
  module M : S = struct type t = int let x = 1 end

  module N = F(M)
end

(* from ocaml/ocaml#13955 no unused warning should be triggered for [test] *)

module I : sig
  module F (_ : sig val test : int end) : sig end
end = struct
 module F (X: sig val test : int end) = struct let _ = X.test end
end

(* same for the recursive version *)

module rec X: sig
  module F(_:sig val x:int end): sig end
end = struct
  module F(X:sig val x:int end) = struct let _ = X.x end
end
and Y: sig end = struct end

(* TEST
 flags = "-w +A";
 setup-ocamlc.byte-build-env;
 module = "w32.mli";
 ocamlc.byte;
 module = "w32.ml";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
