(* TEST
   expect;
*)

let #() = #()
let f #() = #()
let g = fun #() -> #()
let h = function #() -> #()
let i x = match x with #() -> #()
let j (#() as x) = x
let k = fun (#() as x) -> x
let l = function #() as x -> x
let m x = match x with #() -> x

[%%expect{|
val f : unit# -> unit# = <fun>
val g : unit# -> unit# = <fun>
val h : unit# -> unit# = <fun>
val i : unit# -> unit# = <fun>
val j : unit# -> unit# = <fun>
val k : unit# -> unit# = <fun>
val l : unit# -> unit# = <fun>
val m : unit# -> unit# = <fun>
|}]

let f (#() | #()) = #()

[%%expect{|
Line 1, characters 13-16:
1 | let f (#() | #()) = #()
                 ^^^
Warning 12 [redundant-subpat]: this sub-pattern is unused.

val f : unit# -> unit# = <fun>
|}]

let f = function #() -> #() | #() -> #() 

[%%expect{|
Line 1, characters 30-33:
1 | let f = function #() -> #() | #() -> #()
                                  ^^^
Warning 11 [redundant-case]: this match case is unused.

val f : unit# -> unit# = <fun>
|}]

let f = function #() -> #() | _ -> #()

[%%expect{|
Line 1, characters 30-31:
1 | let f = function #() -> #() | _ -> #()
                                  ^
Warning 11 [redundant-case]: this match case is unused.

val f : unit# -> unit# = <fun>
|}]


let f = function #(#(), _) | #(_, #()) -> #() 

[%%expect{|
Line 1, characters 29-38:
1 | let f = function #(#(), _) | #(_, #()) -> #()
                                 ^^^^^^^^^
Warning 12 [redundant-subpat]: this sub-pattern is unused.

val f : #(unit# * unit#) -> unit# = <fun>
|}]

let f = function #(#(), _) -> #() | #(_, #()) -> #()

[%%expect{|
Line 1, characters 36-45:
1 | let f = function #(#(), _) -> #() | #(_, #()) -> #()
                                        ^^^^^^^^^
Warning 11 [redundant-case]: this match case is unused.

val f : #(unit# * unit#) -> unit# = <fun>
|}]

let f = function #(#(), #()) -> #() | _ -> #()

[%%expect{|
Line 1, characters 38-39:
1 | let f = function #(#(), #()) -> #() | _ -> #()
                                          ^
Warning 11 [redundant-case]: this match case is unused.

val f : #(unit# * unit#) -> unit# = <fun>
|}]

let f = function #(#(), #false) -> #()

[%%expect{|
Line 1, characters 8-38:
1 | let f = function #(#(), #false) -> #()
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
#(#(), #true)

val f : #(unit# * bool#) -> unit# = <fun>
|}]

let f = function #(#true, #(#(), #false)) -> #() | #(#false, #(_, #true)) -> #()

[%%expect{|
Line 1, characters 8-80:
1 | let f = function #(#true, #(#(), #false)) -> #() | #(#false, #(_, #true)) -> #()
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
#(#true, #(#(), #true))

val f : #(bool# * #(unit# * bool#)) -> unit# = <fun>
|}]

let f () = match () with #() -> #()

[%%expect{|
Line 1, characters 25-28:
1 | let f () = match () with #() -> #()
                             ^^^
Error: This pattern matches values of type "unit#"
       but a pattern was expected which matches values of type "unit"
|}]

let f #() = match #() with #() -> #()

[%%expect{|
val f : unit# -> unit# = <fun>
|}]

let f = function #() -> #() | () -> #()

[%%expect{|
Line 1, characters 30-32:
1 | let f = function #() -> #() | () -> #()
                                  ^^
Error: This pattern matches values of type "unit"
       but a pattern was expected which matches values of type "unit#"
|}]

let f = function #() -> #() | #() -> ()

[%%expect{|
Line 1, characters 37-39:
1 | let f = function #() -> #() | #() -> ()
                                         ^^
Error: This expression has type "unit" but an expression was expected of type
         "unit#"
|}]

