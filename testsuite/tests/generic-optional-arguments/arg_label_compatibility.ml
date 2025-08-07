(* TEST
flags = "-extension-universe alpha ";

 expect;
*)

(* Comprehensive test suite for argument label compatibility *)

(* CR generic-optional: Test compatibility matrix for all parameter types (Nolabel, ~x, ?x, 
   Option.?'x, Or_null.?'x, ~(x:t)) with all argument types *)

(* CR generic-optional: Test multi-argument functions with mixed types, partial application,
   and argument reordering *)

(* CR generic-optional: Test module inclusion for signatures/structures with different 
   optional argument types *)

(* CR generic-optional: Test higher-order functions taking/returning functions with optional args *)

(* CR generic-optional: Test object/class methods with optional arguments and inheritance *)

(* CR generic-optional: Test edge cases: conflicting paths, empty labels, error messages *)