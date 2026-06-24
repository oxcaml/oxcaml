(* TEST
 expect;
*)

(* Type-checking of [thm_?] theorem declarations is not yet implemented:
   the type-checker raises a located "Theorem declarations are not yet
   supported" error at the item level.  These expect-tests pin that
   behaviour (and its location).

   Note: the spec is *not* descended into, so even a refinement spec that
   would otherwise raise the Part 1 "Refinement types are not yet
   supported" error reports the theorem-level error instead.

   (Syntax / keyword errors for [thm_] cannot be captured here because the
   expect toplevel aborts on a parse error rather than localising it to a
   single block; those are covered by [theorem.ml] instead.) *)

module type S = sig
  thm_? sqrt_1 : {[ sqrt 1. = 1. ]}
end
[%%expect{|
Line 2, characters 2-35:
2 |   thm_? sqrt_1 : {[ sqrt 1. = 1. ]}
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Theorem declarations are not yet supported
|}]

module type T = sig
  thm_? small : (x : float | x < 1.) -> {[ sqrt x > x ]}
end
[%%expect{|
Line 2, characters 2-56:
2 |   thm_? small : (x : float | x < 1.) -> {[ sqrt x > x ]}
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Theorem declarations are not yet supported
|}]

module type U = sig
  thm_? t1 : int -> int
end
[%%expect{|
Line 2, characters 2-23:
2 |   thm_? t1 : int -> int
      ^^^^^^^^^^^^^^^^^^^^^
Error: Theorem declarations are not yet supported
|}]
