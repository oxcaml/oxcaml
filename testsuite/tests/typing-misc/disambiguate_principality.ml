(* TEST
   expect;
*)

(*** Record ***)

(* Expressions *)

module M = struct
  type r = { lbl : int }
end

[%%expect {|
module M : sig type r = { lbl : int; } end
|}]

let before_a : M.r = { lbl = 3 }

[%%expect {|
val before_a : M.r = {M.lbl = 3}
|}]

let a =
  let x = ({ M.lbl = 3 } : M.r) in
  x.lbl

[%%expect {|
val a : int = 3
|}]

let after_a =
  let x = ({ M.lbl = 3 } : M.r) in
  { x with lbl = 4 }

[%%expect
{|
Line 3, characters 2-20:
3 |   { x with lbl = 4 }
      ^^^^^^^^^^^^^^^^^^
Warning 23 [useless-record-with]: all the fields are explicitly listed in this record:
the 'with' clause is useless.

val after_a : M.r = {M.lbl = 4}
|}]

let b =
  let x = ({ contents = { M.lbl = 3 } } : M.r ref) in
  x := { lbl = 4 }

[%%expect
{|
val b : unit = ()
|},
  Principal
    {|
Line 3, characters 7-18:
3 |   x := { lbl = 4 }
           ^^^^^^^^^^^
Warning 18 [not-principal]: this type-based record disambiguation is not principal.

val b : unit = ()
|}]

let c =
  let x = ({ contents = { M.lbl = 3 } } : M.r ref) in
  !x.lbl

[%%expect {|
val c : int = 3
|}]

let d =
  let x = ({ contents = { M.lbl = 3 } } : M.r ref) in
  x.contents <- { lbl = 4 }

[%%expect {|
val d : unit = ()
|}]

let e =
  let x = ({ contents = { M.lbl = 3 } } : M.r ref) in
  { x with contents = { lbl = 4 } }

[%%expect
{|
Line 3, characters 24-27:
3 |   { x with contents = { lbl = 4 } }
                            ^^^
Error: Unbound record field "lbl"
|}]

let f =
  let x = ({ contents = { M.lbl = 3 } } : M.r ref) in
  x.contents.lbl

[%%expect {|
val f : int = 3
|}]

(* Patterns *)

let g (x : M.r) = match x with { lbl = _ } -> ()

[%%expect {|
val g : M.r -> unit = <fun>
|}]

let h x = match x with (_ : M.r) -> () | { lbl = _ } -> ()

[%%expect
{|
Line 1, characters 41-52:
1 | let h x = match x with (_ : M.r) -> () | { lbl = _ } -> ()
                                             ^^^^^^^^^^^
Warning 11 [redundant-case]: this match case is unused.

val h : M.r -> unit = <fun>
|}, Principal{|
Line 1, characters 41-52:
1 | let h x = match x with (_ : M.r) -> () | { lbl = _ } -> ()
                                             ^^^^^^^^^^^
Warning 18 [not-principal]: this type-based record disambiguation is not principal.

Line 1, characters 41-52:
1 | let h x = match x with (_ : M.r) -> () | { lbl = _ } -> ()
                                             ^^^^^^^^^^^
Warning 11 [redundant-case]: this match case is unused.

val h : M.r -> unit = <fun>
|}]

let i x = match x with { lbl = _ } -> () | (_ : M.r) -> ()

[%%expect
{|
Line 1, characters 25-28:
1 | let i x = match x with { lbl = _ } -> () | (_ : M.r) -> ()
                             ^^^
Error: Unbound record field "lbl"
|}]

let j x = match x with (_ : M.r) | { lbl = _ } -> ()

[%%expect
{|
Line 1, characters 35-46:
1 | let j x = match x with (_ : M.r) | { lbl = _ } -> ()
                                       ^^^^^^^^^^^
Warning 12 [redundant-subpat]: this sub-pattern is unused.

val j : M.r -> unit = <fun>
|}, Principal{|
Line 1, characters 35-46:
1 | let j x = match x with (_ : M.r) | { lbl = _ } -> ()
                                       ^^^^^^^^^^^
Warning 18 [not-principal]: this type-based record disambiguation is not principal.

Line 1, characters 35-46:
1 | let j x = match x with (_ : M.r) | { lbl = _ } -> ()
                                       ^^^^^^^^^^^
Warning 12 [redundant-subpat]: this sub-pattern is unused.

val j : M.r -> unit = <fun>
|}]

let k x = match x with { lbl = _ } | (_ : M.r) -> ()

[%%expect
{|
Line 1, characters 25-28:
1 | let k x = match x with { lbl = _ } | (_ : M.r) -> ()
                             ^^^
Error: Unbound record field "lbl"
|}]

let l (x : M.r ref) = match x with { contents = { lbl = _ } } -> ()

[%%expect {|
val l : M.r ref -> unit = <fun>
|}]

let m x = match x with { contents = { lbl = _ } } -> ()

[%%expect
{|
Line 1, characters 38-41:
1 | let m x = match x with { contents = { lbl = _ } } -> ()
                                          ^^^
Error: Unbound record field "lbl"
|}]

let n x = match x with (_ : M.r ref) -> () | { contents = { lbl = _ } } -> ()

[%%expect
{|
Line 1, characters 45-71:
1 | let n x = match x with (_ : M.r ref) -> () | { contents = { lbl = _ } } -> ()
                                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 11 [redundant-case]: this match case is unused.

val n : M.r ref -> unit = <fun>
|}, Principal{|
Line 1, characters 58-69:
1 | let n x = match x with (_ : M.r ref) -> () | { contents = { lbl = _ } } -> ()
                                                              ^^^^^^^^^^^
Warning 18 [not-principal]: this type-based record disambiguation is not principal.

Line 1, characters 45-71:
1 | let n x = match x with (_ : M.r ref) -> () | { contents = { lbl = _ } } -> ()
                                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 11 [redundant-case]: this match case is unused.

val n : M.r ref -> unit = <fun>
|}]

let o x = match x with { contents = { lbl = _ } } -> () | (_ : M.r ref) -> ()

[%%expect
{|
Line 1, characters 38-41:
1 | let o x = match x with { contents = { lbl = _ } } -> () | (_ : M.r ref) -> ()
                                          ^^^
Error: Unbound record field "lbl"
|}]

let p x = match x with (_ : M.r ref) | { contents = { lbl = _ } } -> ()

[%%expect
{|
Line 1, characters 39-65:
1 | let p x = match x with (_ : M.r ref) | { contents = { lbl = _ } } -> ()
                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 12 [redundant-subpat]: this sub-pattern is unused.

val p : M.r ref -> unit = <fun>
|}, Principal{|
Line 1, characters 52-63:
1 | let p x = match x with (_ : M.r ref) | { contents = { lbl = _ } } -> ()
                                                        ^^^^^^^^^^^
Warning 18 [not-principal]: this type-based record disambiguation is not principal.

Line 1, characters 39-65:
1 | let p x = match x with (_ : M.r ref) | { contents = { lbl = _ } } -> ()
                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 12 [redundant-subpat]: this sub-pattern is unused.

val p : M.r ref -> unit = <fun>
|}]

let q x = match x with { contents = { lbl = _ } } | (_ : M.r ref) -> ()

[%%expect
{|
Line 1, characters 38-41:
1 | let q x = match x with { contents = { lbl = _ } } | (_ : M.r ref) -> ()
                                          ^^^
Error: Unbound record field "lbl"
|}]

let r arg = match arg with (x : M.r ref) -> !x.lbl

[%%expect {|
val r : M.r ref -> int = <fun>
|}]

let s arg = match arg with (x : M.r ref) -> x := { lbl = 4 }

[%%expect
{|
val s : M.r ref -> unit = <fun>
|}, Principal{|
Line 1, characters 49-60:
1 | let s arg = match arg with (x : M.r ref) -> x := { lbl = 4 }
                                                     ^^^^^^^^^^^
Warning 18 [not-principal]: this type-based record disambiguation is not principal.

val s : M.r ref -> unit = <fun>
|}]

let t = function
  | ({ contents = { M.lbl = _ } } : M.r ref) as x -> x := { lbl = 4 }

[%%expect
{|
val t : M.r ref -> unit = <fun>
|}, Principal{|
Line 2, characters 58-69:
2 |   | ({ contents = { M.lbl = _ } } : M.r ref) as x -> x := { lbl = 4 }
                                                              ^^^^^^^^^^^
Warning 18 [not-principal]: this type-based record disambiguation is not principal.

val t : M.r ref -> unit = <fun>
|}]

let u = function ({ contents = { M.lbl = _ } } : M.r ref) as x -> !x.lbl

[%%expect {|
val u : M.r ref -> int = <fun>
|}]

(*** Constructors ***)

(* Expressions *)

module M = struct
  type t =
    | A
    | B
end

[%%expect {|
module M : sig type t = A | B end
|}]

let before_a : M.t = A

[%%expect {|
val before_a : M.t = M.A
|}]

let a =
  let x = (A : M.t) in
  x

[%%expect {|
val a : M.t = M.A
|}]

let b =
  let x = ({ contents = A } : M.t ref) in
  x := B

[%%expect
{|
val b : unit = ()
|},
  Principal
    {|
Line 3, characters 7-8:
3 |   x := B
           ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not principal.

val b : unit = ()
|}]

let d =
  let x = ({ contents = A } : M.t ref) in
  x.contents <- B

[%%expect {|
val d : unit = ()
|}]

let e =
  let x = ({ contents = A } : M.t ref) in
  { x with contents = B }

[%%expect
{|
Line 3, characters 22-23:
3 |   { x with contents = B }
                          ^
Error: Unbound constructor "B"
|}]

(* Patterns *)

let g (x : M.t) = match x with A | B -> ()

[%%expect {|
val g : M.t -> unit = <fun>
|}]

let h x = match x with (A : M.t) -> () | B -> ()

[%%expect
{|
val h : M.t -> unit = <fun>
|}, Principal{|
Line 1, characters 41-42:
1 | let h x = match x with (A : M.t) -> () | B -> ()
                                             ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not principal.

val h : M.t -> unit = <fun>
|}]

let i x = match x with A -> () | (B : M.t) -> ()

[%%expect
{|
Line 1, characters 23-24:
1 | let i x = match x with A -> () | (B : M.t) -> ()
                           ^
Error: Unbound constructor "A"
|}]

let j x = match x with (A : M.t) | B -> ()

[%%expect
{|
val j : M.t -> unit = <fun>
|}, Principal{|
Line 1, characters 35-36:
1 | let j x = match x with (A : M.t) | B -> ()
                                       ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not principal.

val j : M.t -> unit = <fun>
|}]

let k x = match x with A | (B : M.t) -> ()

[%%expect
{|
Line 1, characters 23-24:
1 | let k x = match x with A | (B : M.t) -> ()
                           ^
Error: Unbound constructor "A"
|}]

let l (x : M.t ref) = match x with { contents = A | B } -> ()

[%%expect {|
val l : M.t ref -> unit = <fun>
|}]

let m x = match x with { contents = A | B } -> ()

[%%expect
{|
Line 1, characters 36-37:
1 | let m x = match x with { contents = A | B } -> ()
                                        ^
Error: Unbound constructor "A"
|}]

let n x = match x with (_ : M.t ref) -> () | { contents = A } -> ()

[%%expect
{|
Line 1, characters 45-61:
1 | let n x = match x with (_ : M.t ref) -> () | { contents = A } -> ()
                                                 ^^^^^^^^^^^^^^^^
Warning 11 [redundant-case]: this match case is unused.

val n : M.t ref -> unit = <fun>
|}, Principal{|
Line 1, characters 58-59:
1 | let n x = match x with (_ : M.t ref) -> () | { contents = A } -> ()
                                                              ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not principal.

Line 1, characters 45-61:
1 | let n x = match x with (_ : M.t ref) -> () | { contents = A } -> ()
                                                 ^^^^^^^^^^^^^^^^
Warning 11 [redundant-case]: this match case is unused.

val n : M.t ref -> unit = <fun>
|}]

let o x = match x with { contents = A } -> () | (_ : M.t ref) -> ()

[%%expect
{|
Line 1, characters 36-37:
1 | let o x = match x with { contents = A } -> () | (_ : M.t ref) -> ()
                                        ^
Error: Unbound constructor "A"
|}]

let p x = match x with (_ : M.t ref) | { contents = A } -> ()

[%%expect
{|
Line 1, characters 39-55:
1 | let p x = match x with (_ : M.t ref) | { contents = A } -> ()
                                           ^^^^^^^^^^^^^^^^
Warning 12 [redundant-subpat]: this sub-pattern is unused.

val p : M.t ref -> unit = <fun>
|}, Principal{|
Line 1, characters 52-53:
1 | let p x = match x with (_ : M.t ref) | { contents = A } -> ()
                                                        ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not principal.

Line 1, characters 39-55:
1 | let p x = match x with (_ : M.t ref) | { contents = A } -> ()
                                           ^^^^^^^^^^^^^^^^
Warning 12 [redundant-subpat]: this sub-pattern is unused.

val p : M.t ref -> unit = <fun>
|}]

let q x = match x with { contents = A } | (_ : M.t ref) -> ()

[%%expect
{|
Line 1, characters 36-37:
1 | let q x = match x with { contents = A } | (_ : M.t ref) -> ()
                                        ^
Error: Unbound constructor "A"
|}]

let s arg = match arg with (x : M.t ref) -> x := A

[%%expect
{|
val s : M.t ref -> unit = <fun>
|}, Principal{|
Line 1, characters 49-50:
1 | let s arg = match arg with (x : M.t ref) -> x := A
                                                     ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not principal.

val s : M.t ref -> unit = <fun>
|}]

let t = function ({ contents = M.A } : M.t ref) as x -> x := B

[%%expect
{|
Line 1, characters 8-62:
1 | let t = function ({ contents = M.A } : M.t ref) as x -> x := B
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
{contents=B}

val t : M.t ref -> unit = <fun>
|}, Principal{|
Line 1, characters 61-62:
1 | let t = function ({ contents = M.A } : M.t ref) as x -> x := B
                                                                 ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not principal.

Line 1, characters 8-62:
1 | let t = function ({ contents = M.A } : M.t ref) as x -> x := B
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
{contents=B}

val t : M.t ref -> unit = <fun>
|}]
