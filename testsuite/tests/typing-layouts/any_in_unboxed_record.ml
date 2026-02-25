(* TEST
 include stdlib_stable;
 flags = "-extension layouts_alpha";
 expect;
*)

open Stdlib_stable

type ('a : any) t = { fst : 'a; mutable snd : 'a }
[%%expect{|
type ('a : any) t = { fst : 'a; mutable snd : 'a; }
|}]

let fst t = t.#fst
[%%expect{|
val fst : 'a t# -> 'a = <fun>
|}]

let fst (type a : any) (t : a t#) = t.#fst
[%%expect{|
Line 1, characters 23-33:
1 | let fst (type a : any) (t : a t#) = t.#fst
                           ^^^^^^^^^^
Error: This pattern matches values of type "a t#"
       but a pattern was expected which matches values of type
         "('a : '_representable_layout_1 & '_representable_layout_2)"
       The layout of a t# is any & any
         because it is an unboxed record.
       But the layout of a t# must be representable
         because we must know concretely how to pass a function argument.
|}]

let fst (t : int t#) = t.#fst
[%%expect{|
val fst : int t# -> int = <fun>
|}]

let fst (type a : value) (t : a t#) = t.#fst
[%%expect{|
val fst : 'a t# -> 'a = <fun>
|}]

let fst (t : int64# t#) = t.#fst
[%%expect{|
val fst : int64# t# -> int64# = <fun>
|}]

let fst (t : int64# t#) =
  match t with #{ fst; _ } -> fst
[%%expect{|
val fst : int64# t# -> int64# = <fun>
|}]

let fst (type a : bits64) (t : a t#) = t.#fst
[%%expect{|
val fst : ('a : bits64). 'a t# -> 'a = <fun>
|}]

let fst (type a : bits64) (t : a t#) =
  match t with #{ fst; _ } -> fst
[%%expect{|
val fst : ('a : bits64). 'a t# -> 'a = <fun>
|}]

let make fst snd = #{ fst; snd }
[%%expect{|
val make : 'a -> 'a -> 'a t# = <fun>
|}]

let make (fst : int) snd = #{ fst; snd }
[%%expect{|
val make : int -> int -> int t# = <fun>
|}]

let make (fst : int) (snd : int) = #{ fst; snd }
[%%expect{|
val make : int -> int -> int t# = <fun>
|}]

let make fst snd : int t# = #{ fst; snd }
[%%expect{|
val make : int -> int -> int t# = <fun>
|}]

let make (type a : value) (fst : a) (snd : a) = #{ fst; snd }
[%%expect{|
val make : 'a -> 'a -> 'a t# = <fun>
|}]

let make (type a : value) fst snd : a t# = #{ fst; snd }
[%%expect{|
val make : 'a -> 'a -> 'a t# = <fun>
|}]

let make (fst : int64#) snd = #{ fst; snd }
[%%expect{|
val make : int64# -> int64# -> int64# t# = <fun>
|}]

let make (fst : int64#) (snd : int64#) = #{ fst; snd }
[%%expect{|
val make : int64# -> int64# -> int64# t# = <fun>
|}]

let make (type a : bits64) (fst : a) (snd : a) = #{ fst; snd }
[%%expect{|
val make : ('a : bits64). 'a -> 'a -> 'a t# = <fun>
|}]

external box_int64 : int64# -> int64 = "%box_int64"
[%%expect {|
external box_int64 : int64# -> int64 = "%box_int64"
|}]

let[@inline never] make_test_value () = #{ fst = #1L; snd = #2L }
[%%expect {|
val make_test_value : unit -> int64# t# = <fun>
|}]

let test_direct =
  (* Check that projecting from a block with an [any] accounts for the layouts
     of all previous fields in the block *)
  (make_test_value ()).#snd |> box_int64
[%%expect{|
val test_direct : int64 = 2L
|}]

let[@inline never] make_unboxed_pair_value () : #(int64# * int64#) t# =
  #{ fst = #(#1L, #2L); snd = #(#3L, #4L) }
[%%expect {|
val make_unboxed_pair_value : unit -> #(int64# * int64#) t# = <fun>
|}]

let test_unboxed_pair_direct =
  let #(_fst, snd) = (make_unboxed_pair_value ()).#snd in snd |> box_int64
[%%expect{|
val test_unboxed_pair_direct : int64 = 4L
|}]

let[@inline never] make_nested_value () : int64# t# t# =
  #{ fst = #{ fst = #1L; snd = #2L }; snd = #{ fst = #3L; snd = #4L } }
[%%expect {|
val make_nested_value : unit -> int64# t# t# = <fun>
|}]

let test_nested_direct =
  (make_nested_value ()).#snd.#snd |> box_int64
[%%expect{|
val test_nested_direct : int64 = 4L
|}]
