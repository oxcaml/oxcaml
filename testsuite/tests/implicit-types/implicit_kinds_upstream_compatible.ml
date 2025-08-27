(* TEST
 flags = "-extension-universe upstream_compatible";
 expect;
*)

(* Implicit kinds are upstream-compatible. *)

module type S = sig
  [@@@implicit_kind: ('elt : bits64)]

  val f : 'elt -> 'elt array
end

[%%expect{|
module type S = sig val f : ('elt : bits64). 'elt -> 'elt array end
|}]
