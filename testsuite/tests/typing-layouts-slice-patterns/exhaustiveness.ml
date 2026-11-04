(* TEST
   expect;
*)

(* Lengths above the fixed lengths *)
let f = function [&| |] -> 0 | [&| _ |] -> 1
[%%expect{|
Line 1, characters 8-44:
1 | let f = function [&| |] -> 0 | [&| _ |] -> 1
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "[&| _ ; _ |]"

val f : 'a slice -> int = <fun>
|}]

(* Lengths below the fixed lengths *)
let f = function [&: _ :] -> 0 | [&: _; _ :] -> 1
[%%expect{|
Line 1, characters 8-49:
1 | let f = function [&: _ :] -> 0 | [&: _; _ :] -> 1
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "[&: :]"

val f : 'a slice_imm -> int = <fun>
|}]

(* Patterns of different lengths don't make each other unused *)
let f = function [&| 0 |] -> 0 | [&| 0; _ |] -> 1 | [&| 0 |] -> 2 | _ -> 3
[%%expect{|
Line 1, characters 52-60:
1 | let f = function [&| 0 |] -> 0 | [&| 0; _ |] -> 1 | [&| 0 |] -> 2 | _ -> 3
                                                        ^^^^^^^^
Warning 11 [redundant-case]: this match case is unused.

val f : int slice -> int = <fun>
|}]
