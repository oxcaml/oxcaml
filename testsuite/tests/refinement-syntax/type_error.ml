(* TEST
 expect;
*)

(* Refinements are only allowed at the argument / result positions of a
   theorem spec ([thm_? ...]).  Used in an ordinary type (here a value's
   type), the type-checker rejects every refinement surface form with a
   located error.  These expect-tests pin that behaviour (and its location)
   for each form. *)

let f (x : (int | true)) = x
[%%expect{|
Line 1, characters 11-23:
1 | let f (x : (int | true)) = x
               ^^^^^^^^^^^^
Error: Refinements are only allowed at theorem argument/result positions
|}]

let g (x : (y : int | y > 0)) = x
[%%expect{|
Line 1, characters 11-28:
1 | let g (x : (y : int | y > 0)) = x
               ^^^^^^^^^^^^^^^^^
Error: Refinements are only allowed at theorem argument/result positions
|}]

let h (x : {[ true ]}) = x
[%%expect{|
Line 1, characters 11-21:
1 | let h (x : {[ true ]}) = x
               ^^^^^^^^^^
Error: Refinements are only allowed at theorem argument/result positions
|}]
