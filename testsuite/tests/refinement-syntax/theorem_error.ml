(* TEST
 expect;
*)

(* [thm_?] theorem declarations are gated behind the [refinements] language
   extension.  This file deliberately OMITS [-extension refinements] in its
   TEST stanza, so every theorem declaration is rejected at the item level
   with the "extension disabled" error.  These expect-tests pin that
   behaviour (and its location).

   The error fires before the spec is descended into, so even a spec that
   would otherwise be a type error reports the extension-disabled error
   instead.  (Positive typechecking and spec-level errors, all with the
   extension enabled, live in [typecheck.ml] / [typecheck_error.ml].)

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
Error: The extension "refinements" is disabled and cannot be used
|}]

module type T = sig
  thm_? small : (x : float | x < 1.) -> {[ sqrt x > x ]}
end
[%%expect{|
Line 2, characters 2-56:
2 |   thm_? small : (x : float | x < 1.) -> {[ sqrt x > x ]}
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The extension "refinements" is disabled and cannot be used
|}]

module type U = sig
  thm_? t1 : int -> int
end
[%%expect{|
Line 2, characters 2-23:
2 |   thm_? t1 : int -> int
      ^^^^^^^^^^^^^^^^^^^^^
Error: The extension "refinements" is disabled and cannot be used
|}]
