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

(* All usages of immediate and immediate64 are allowed. *)

module type S2 = sig
  [@@@implicit_kind: ('elt : immediate)]

  val g : 'elt array -> int
end
[%%expect{|
<<<<<<< HEAD
Line 4, characters 2-27:
4 |   val g : 'elt array -> int
      ^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 187 [incompatible-with-upstream]: Usage of layout immediate/immediate64 in g
  can't be erased for compatibility with upstream OCaml.

||||||| 9790921724
Line 4, characters 2-27:
4 |   val g : 'elt array -> int
      ^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 187 [incompatible-with-upstream]: Usage of layout immediate/immediate64 in g
can't be erased for compatibility with upstream OCaml.

=======
>>>>>>> 5.2.0minus-37
module type S2 = sig val g : ('elt : immediate). 'elt array -> int end
|}]
