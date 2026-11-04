(* TEST
   expect;
*)

(* The empty slice and slices with at least one element are all slices *)
let f = function [&| |] -> 0 | [&| _; .. |] -> 1
[%%expect{|
val f : 'a slice -> int = <fun>
|}]

(* Lengths below the minimum length of the variable-length patterns *)
let f = function [&: :] -> 0 | [&: _; _; .. :] -> 1
[%%expect{|
Line 1, characters 8-51:
1 | let f = function [&: :] -> 0 | [&: _; _; .. :] -> 1
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "[&: _ :]"

val f : 'a slice_imm -> int = <fun>
|}]

(* Lengths above the fixed lengths *)
let f = function [&| |] -> 0 | [&| _ |] -> 1
[%%expect{|
Line 1, characters 8-44:
1 | let f = function [&| |] -> 0 | [&| _ |] -> 1
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "[&| _ ; _ ; .. |]"

val f : 'a slice -> int = <fun>
|}]

(* Fixed lengths beyond the prefixes are distinguished *)
let f = function [&| 0; .. |] -> 0 | [&| _; _; _ |] -> 1
[%%expect{|
Line 1, characters 8-56:
1 | let f = function [&| 0; .. |] -> 0 | [&| _; _; _ |] -> 1
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "[&| 1 ; _ ; _ ; _ ; .. |]"

val f : int slice -> int = <fun>
|}]

(* A variable-length pattern can be unused *)
let f = function [&| _ |] -> 0 | [&| _; _; .. |] -> 1 | [&| _; .. |] -> 2
[%%expect{|
Line 1, characters 8-73:
1 | let f = function [&| _ |] -> 0 | [&| _; _; .. |] -> 1 | [&| _; .. |] -> 2
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "[&| |]"

Line 1, characters 56-68:
1 | let f = function [&| _ |] -> 0 | [&| _; _; .. |] -> 1 | [&| _; .. |] -> 2
                                                            ^^^^^^^^^^^^
Warning 11 [redundant-case]: this match case is unused.

val f : 'a slice -> int = <fun>
|}]

(* A sub-pattern is only unused if it is unused for all lengths *)
let f = function [&| 0; _ |] -> 0 | [&| (0 | 1); .. |] -> 1 | _ -> 2
[%%expect{|
val f : int slice -> int = <fun>
|}]

(* Warning 57: the rest is bound to different slices *)
let f g = function
  | ([&| 0; .. as r |] | [&| .. as r |]) when g r -> 0
  | _ -> 1
[%%expect{|
Line 2, characters 4-40:
2 |   | ([&| 0; .. as r |] | [&| .. as r |]) when g r -> 0
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 57 [ambiguous-var-in-pattern-guard]: Ambiguous or-pattern variables under
  guard; variable "r" appears in different places in different or-pattern
  alternatives.
  Only the first match will be used to evaluate the guard expression.
  (see manual section 13.5.4)

val f : (int slice -> bool) -> int slice -> int = <fun>
|}]

let f g = function
  | ([&| 0; .. as r |] | [&| 1; .. as r |]) when g r -> 0
  | _ -> 1
[%%expect{|
val f : (int slice -> bool) -> int slice -> int = <fun>
|}]
