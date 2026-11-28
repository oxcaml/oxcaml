(* TEST
 flags += "-strict-sequence";
 { expect; }{ expect.opt; }
*)

(* Helper functions. Notice how some return unit# and some return unit *)
let ( := ) r c = r.contents <- c; #()
let incr r = r := !r + 1
let double r = r.contents <- 2 * !r

type non_unit_void : void
external non_unit_void : unit -> non_unit_void = "%unbox_unit"
[%%expect{|
val ( := ) : 'a ref -> 'a -> unit# = <fun>
val incr : int ref -> unit# = <fun>
val double : int ref -> unit = <fun>
type non_unit_void : void
external non_unit_void : unit -> non_unit_void = "%unbox_unit"
|}]

(* Disambiguation of unit/unit# in sequences *)
let x =
  let x = ref 0 in
  x := !x + 10;
  double x;
  incr x;
  double x;
  !x
[%%expect{|
val x : int = 42
|}]

(* Disambiguation of unit/unit# in for loops *)
let x =
  let total = ref 0 in
  for i = 1 to 10 do
    total := !total + i
  done;
  !total
[%%expect{|
val x : int = 55
|}]

(* Disambiguation of unit/unit# in while loops *)
let x =
  let total = ref 0 in
  let i = ref 0 in
  while incr i; !i <= 10 do
    total := !total + !i
  done;
  !total
[%%expect{|
val x : int = 55
|}]

(* Disambiguation of unit/unit# in if statements *)
let incr_if b r =
  if b then incr r;
  !r
let use_unit_u_if b x =
  let #() = if b then x in
  #()
let synthesized_unit_u = if Sys.opaque_identity false then #()
[%%expect{|
val incr_if : bool -> int ref -> int = <fun>
val use_unit_u_if : bool -> unit# -> unit# = <fun>
val synthesized_unit_u : unit# = <abstr>
|}]

(* Disambiguation of unit/unit# in if statements is upstream-incompatible since
   [ty_expected = unit] no longer happens when typechecking [ifso] branches *)
type _ t = Unit : unit t

let f (type a) (t : a t) (x : a) =
  if true then (match t with Unit -> x)
[%%expect{|
type _ t = Unit : unit t
Line 4, characters 15-39:
4 |   if true then (match t with Unit -> x)
                   ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This "match" expression has type "a"
       but an expression was expected of type "unit"
       because it is in the result of a conditional with no else branch
|}]

include struct [@@@warning "-redefining-unit"] type t = () end
let x = if true then ()
[%%expect{|
type t = ()
Line 2, characters 21-23:
2 | let x = if true then ()
                         ^^
Error: The constructor "()" has type "t" but an expression was expected of type
         "unit"
       because it is in the result of a conditional with no else branch
|}]

(* Principality *)
let g #() = #()
let f x = g x; x; 42
let h b x = g x; if b then x
[%%expect{|
val g : unit# -> unit# = <fun>
val f : unit# -> int = <fun>
val h : bool -> unit# -> unit# = <fun>
|}, Principal{|
val g : unit# -> unit# = <fun>
Line 2, characters 15-16:
2 | let f x = g x; x; 42
                   ^
Warning 18 [not-principal]: this type-based unit# disambiguation is not
  principal.

val f : unit# -> int = <fun>
Line 3, characters 17-28:
3 | let h b x = g x; if b then x
                     ^^^^^^^^^^^
Warning 18 [not-principal]: this type-based unit# disambiguation is not
  principal.

val h : bool -> unit# -> unit# = <fun>
|}]

(* The previous example is analogous to: *)
type t1 = A
type t2 = A

let g (A : t1) = (A : t1)
let f x =
  match g x with
  | A ->
    match x with
    | A -> 42
[%%expect{|
type t1 = A
type t2 = A
val g : t1 -> t1 = <fun>
val f : t1 -> int = <fun>
|}, Principal{|
type t1 = A
type t2 = A
val g : t1 -> t1 = <fun>
Line 9, characters 6-7:
9 |     | A -> 42
          ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

val f : t1 -> int = <fun>
|}]

let f b c = if b then #() else if c then #()
[%%expect{|
val f : bool -> bool -> unit# = <fun>
|}]

(* Can't disambiguate to arbitrary void type *)
let x = non_unit_void (); 42
[%%expect{|
Line 1, characters 8-24:
1 | let x = non_unit_void (); 42
            ^^^^^^^^^^^^^^^^
Error: This expression has type "non_unit_void"
       but an expression was expected of type "unit"
       because it is in the left-hand side of a sequence
|}]

let x = if true then non_unit_void ()
[%%expect{|
Line 1, characters 21-37:
1 | let x = if true then non_unit_void ()
                         ^^^^^^^^^^^^^^^^
Error: This expression has type "non_unit_void"
       but an expression was expected of type "unit"
       because it is in the result of a conditional with no else branch
|}]

let x = for i = 0 to 1 do non_unit_void () done; 42
[%%expect{|
Line 1, characters 26-42:
1 | let x = for i = 0 to 1 do non_unit_void () done; 42
                              ^^^^^^^^^^^^^^^^
Error: This expression has type "non_unit_void"
       but an expression was expected of type "unit"
       because it is in the body of a for-loop
|}]

let x = while false do non_unit_void () done; 42
[%%expect{|
Line 1, characters 23-39:
1 | let x = while false do non_unit_void () done; 42
                           ^^^^^^^^^^^^^^^^
Error: This expression has type "non_unit_void"
       but an expression was expected of type "unit"
       because it is in the body of a while-loop
|}]
