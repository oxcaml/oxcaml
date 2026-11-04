(* TEST
   expect;
*)

(****************************************)
(* Basic typechecking of slice patterns *)

(* The rest of a slice pattern is a slice of the same type *)
let f = function [&| x; .. as r |] -> #(x, r) | _ -> assert false
let f_imm = function [&: x; .. as r :] -> #(x, r) | _ -> assert false
[%%expect{|
val f : 'a slice -> #('a * 'a slice) = <fun>
val f_imm : 'a slice_imm -> #('a * 'a slice_imm) = <fun>
|}]

(* Elements after the rest are not supported yet *)
let bad = function [&| x; .. as r; y |] -> #(x, r, y) | _ -> assert false
[%%expect{|
Line 1, characters 35-36:
1 | let bad = function [&| x; .. as r; y |] -> #(x, r, y) | _ -> assert false
                                       ^
Error: Elements after the rest of a slice pattern are not supported yet.
|}]

(* [[&| ... |]] only matches mutable slices *)
let bad (s : int slice_imm) = match s with [&| _ |] -> () | _ -> ()
[%%expect{|
Line 1, characters 43-51:
1 | let bad (s : int slice_imm) = match s with [&| _ |] -> () | _ -> ()
                                               ^^^^^^^^
Error: This pattern matches values of type "'a slice"
       but a pattern was expected which matches values of type "int slice_imm"
|}]

(* The rest can be bound on both sides of an or-pattern, at different
   positions *)
let f = function
  | [&| 0; .. as r |] | [&| 1; 2; .. as r |] -> r
  | _ -> assert false
[%%expect{|
val f : int slice -> int slice = <fun>
|}]

(* An unused variable bound by a rest is reported like an unused alias *)
let unused_rest (s : int slice) = match s with [&| x; .. as r |] -> x | _ -> 0
[%%expect{|
Line 1, characters 60-61:
1 | let unused_rest (s : int slice) = match s with [&| x; .. as r |] -> x | _ -> 0
                                                                ^
Warning 26 [unused-var]: unused variable "r".

val unused_rest : int slice -> int = <fun>
|}]

(***********)
(* Layouts *)

(* The elements of a slice must be representable *)
let bad : ('a : any). 'a slice -> unit = function [&| _ |] -> () | _ -> ()
[%%expect{|
Line 1, characters 41-74:
1 | let bad : ('a : any). 'a slice -> unit = function [&| _ |] -> () | _ -> ()
                                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This definition has type "'b slice -> unit" which is less general than
         "('a : any). 'a slice -> unit"
       The layout of 'a is any
         because of the annotation on the universal variable 'a.
       But the layout of 'a must be representable
         because it's the type of an element
         in a slice pattern.
|}]

(*********)
(* Modes *)

let use_global : 'a -> unit = fun _ -> ()
let consume : 'a @ unique -> unit = fun _ -> ()
let consume_slice : 'a slice @ unique -> unit = fun _ -> ()
let consume_slice_imm : 'a slice_imm @ unique -> unit = fun _ -> ()
[%%expect{|
val use_global : 'a -> unit = <fun>
val consume : 'a @ unique -> unit = <fun>
val consume_slice : ('a : any). 'a slice @ unique -> unit = <fun>
val consume_slice_imm : ('a : any). 'a slice_imm @ unique -> unit = <fun>
|}]

(* Elements have the locality of the slice *)
let bad (s : string slice @ local) =
  match s with [&| x |] -> use_global x | _ -> ()
[%%expect{|
Line 2, characters 38-39:
2 |   match s with [&| x |] -> use_global x | _ -> ()
                                          ^
Error: This value is "local" to the parent region
         because it is an element of the slice at line 2, characters 15-23
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]

(* Elements of mutable slices are aliased, elements of immutable slices have
   the uniqueness of the slice *)
let bad (s : string slice @ unique) =
  match s with [&| x |] -> consume x | _ -> ()
[%%expect{|
Line 2, characters 35-36:
2 |   match s with [&| x |] -> consume x | _ -> ()
                                       ^
Error: This value is "aliased"
         because it is an element (with some modality) of the slice at line 2, characters 15-23.
       However, the highlighted expression is expected to be "unique".
|}]

let ok (s : string slice_imm @ unique) =
  match s with [&: x :] -> consume x | _ -> ()
[%%expect{|
val ok : string slice_imm @ unique -> unit = <fun>
|}]

(* The rest has the mode of the slice *)
let ok (s : string slice @ unique) =
  match s with [&| _; .. as r |] -> consume_slice r | _ -> ()
[%%expect{|
val ok : string slice @ unique -> unit = <fun>
|}]

(* Reading the elements of a mutable slice requires [read] *)
let bad (s : int slice @ write) =
  match s with [&| _ |] -> () | _ -> ()
[%%expect{|
Line 2, characters 15-23:
2 |   match s with [&| _ |] -> () | _ -> ()
                   ^^^^^^^^
Error: This value is "write"
       but is expected to be "read" or "read_write"
         because its elements is being read.
|}]

(************************)
(* Dereference patterns *)

(* Elements of slices are at addresses of the slice's mutability *)
let f = function [&| deref_ a |] -> a | _ -> assert false
let f_imm = function [&: deref_ a :] -> a | _ -> assert false
[%%expect{|
val f : 'a slice -> 'a addr = <fun>
val f_imm : 'a slice_imm -> 'a addr_imm = <fun>
|}]

(* Dereference patterns can be nested in unboxed elements *)
type ur = #{ i : int; s : string }
let f (s : ur slice) =
  match s with [&| #{ i = deref_ a; _ } |] -> a | _ -> assert false
[%%expect{|
type ur = #{ i : int; s : string; }
val f : ur slice -> int addr = <fun>
|}]

(**************)
(* Uniqueness *)

(* CR slice-patterns: The uniqueness analysis doesn't distinguish the elements
   and the rest of a slice yet. *)
let imprecise (s : string slice_imm @ unique) =
  match s with
  | [&: x; .. as r :] -> consume x; consume_slice_imm r
  | _ -> ()
[%%expect{|
Line 3, characters 54-55:
3 |   | [&: x; .. as r :] -> consume x; consume_slice_imm r
                                                          ^
Error: This value is used here, but it has already been used as unique at:
Line 3, characters 33-34:
3 |   | [&: x; .. as r :] -> consume x; consume_slice_imm r
                                     ^

|}]

(***************)
(* Translation *)

(* CR slice-patterns: Slice patterns are not compiled yet *)
external make : #(#(int array * int64_u) * nativeint_u) -> int slice =
  "%identity"
let f (s : int slice) = match s with [&| x |] -> x | _ -> 0
let _ = f (make #(#([| 1 |], #0L), #1n))
[%%expect{|
external make : #(#(int array * int64_u) * nativeint_u) -> int slice
  = "%identity"
val f : int slice -> int = <fun>
Exception: Failure "Slice patterns are not compiled yet".
|}]
