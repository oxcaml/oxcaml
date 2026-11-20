(* TEST
   expect;
*)

(* Regression tests for oxcaml/oxcaml#8118: [Parmatch.lub] was missing cases
   for unboxed records, [#()] and [#true]/[#false], which caused or-patterns
   containing them to be compiled incorrectly. *)

(* Guarded or-patterns over an unboxed record. *)
type t = #{ a : int; b : bool }

let f (c : t) (g : int -> bool) =
  match c with
  | (#{ a = x; b = true } | #{ a = x; b = false }) when g x -> 1
  | (#{ a = y; b = true } | #{ a = y; b = false }) when g y -> 2
  | _ -> 3

let result = f #{ a = 5; b = true } (fun _ -> false)
[%%expect{|
type t = #{ a : int; b : bool; }
val f : t -> (int -> bool) -> int = <fun>
val result : int = 3
|}]

(* The same, for an implicit unboxed record. *)
type r = { c : int; d : bool }

let f (c : r#) (g : int -> bool) =
  match c with
  | (#{ c = x; d = true } | #{ c = x; d = false }) when g x -> 1
  | (#{ c = y; d = true } | #{ c = y; d = false }) when g y -> 2
  | _ -> 3

let result = f #{ c = 5; d = true } (fun _ -> false)
[%%expect{|
type r = { c : int; d : bool; }
val f : r# -> (int -> bool) -> int = <fun>
val result : int = 3
|}]

(* The same, for [#()]. *)
let f (c : #(int * unit# * bool)) (g : int -> bool) =
  match c with
  | (#(x, #(), true) | #(x, #(), false)) when g x -> 1
  | (#(y, #(), true) | #(y, #(), false)) when g y -> 2
  | _ -> 3

let result = f #(5, #(), true) (fun _ -> false)
[%%expect{|
val f : #(int * unit# * bool) -> (int -> bool) -> int = <fun>
val result : int = 3
|}]

(* An or-pattern over an unboxed record that caused a test on another
   component of the scrutinee to be skipped. *)
type u = #{ a : int }

let f (c : #(bool * u * bool)) =
  match c with
  | #(true, #{ a = 0 }, _) -> 1
  | #(false, _, true) -> 2
  | #(_, (#{ a = 0 } | #{ a = _ }), false) -> 3
  | _ -> 4

let result = f #(true, #{ a = 1 }, true)
[%%expect{|
type u = #{ a : int; }
val f : #(bool * u * bool) -> int = <fun>
val result : int = 4
|}]
