(* TEST
   expect;
*)

let (#false | #true) = #false
let (#true | #false) = #true
let f (#false | #true) = #false
let f (#true | #false) = #true
let g = fun (#false | #true) -> #false
let g = fun (#true | #false) -> #true
let h = function #false -> #true | #true -> #false
let i x = match x with #true -> #false | #false -> #true
let j ((#false | #true) as x) = x
let k = fun ((#true | #false) as x) -> x
let l = function (#false as x) | (#true as x) -> x
let m x = match x with #true | #false -> x 

[%%expect{|
val f : bool# -> bool# = <fun>
val f : bool# -> bool# = <fun>
val g : bool# -> bool# = <fun>
val g : bool# -> bool# = <fun>
val h : bool# -> bool# = <fun>
val i : bool# -> bool# = <fun>
val j : bool# -> bool# = <fun>
val k : bool# -> bool# = <fun>
val l : bool# -> bool# = <fun>
val m : bool# -> bool# = <fun>
|}]

let f (#false | #false) = #true

[%%expect{|
Line 1, characters 6-23:
1 | let f (#false | #false) = #true
          ^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
#true

Line 1, characters 16-22:
1 | let f (#false | #false) = #true
                    ^^^^^^
Warning 12 [redundant-subpat]: this sub-pattern is unused.

val f : bool# -> bool# = <fun>
|}]

let f = function #true -> #false | #true -> #false

[%%expect{|
Line 1, characters 8-50:
1 | let f = function #true -> #false | #true -> #false
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
#false

Line 1, characters 35-40:
1 | let f = function #true -> #false | #true -> #false
                                       ^^^^^
Warning 11 [redundant-case]: this match case is unused.

val f : bool# -> bool# = <fun>
|}]

let f = function #false -> #true | #true -> #false | b -> b

[%%expect{|
Line 1, characters 53-54:
1 | let f = function #false -> #true | #true -> #false | b -> b
                                                         ^
Warning 11 [redundant-case]: this match case is unused.

val f : bool# -> bool# = <fun>
|}]

let f () = match false with #false -> #true | #true -> #false

[%%expect{|
Line 1, characters 28-34:
1 | let f () = match false with #false -> #true | #true -> #false
                                ^^^^^^
Error: This pattern matches values of type "bool#"
       but a pattern was expected which matches values of type "bool"
|}]

let f = function #true -> #false | #false -> true

[%%expect{|
Line 1, characters 45-49:
1 | let f = function #true -> #false | #false -> true
                                                 ^^^^
Error: This expression has type "bool" but an expression was expected of type
         "bool#"
|}]
