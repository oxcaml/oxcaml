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

let fst t = t.fst
[%%expect{|
val fst : 'a t -> 'a = <fun>
|}]

let fst (type a : any) (t : a t) = t.fst
[%%expect{|
Line 1, characters 35-40:
1 | let fst (type a : any) (t : a t) = t.fst
                                       ^^^^^
Error: Fields being projected must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the type of a field being projected.
|}]

let fst (t : int t) = t.fst
[%%expect{|
val fst : int t -> int = <fun>
|}]

let fst (type a : value) (t : a t) = t.fst
[%%expect{|
val fst : 'a t -> 'a = <fun>
|}]

let fst (t : int64# t) = t.fst
[%%expect{|
val fst : int64# t -> int64# = <fun>
|}]

let fst (t : int64# t) =
  match t with { fst; _ } -> fst
[%%expect{|
val fst : int64# t -> int64# = <fun>
|}]

let fst (type a : bits64) (t : a t) = t.fst
[%%expect{|
val fst : ('a : bits64). 'a t -> 'a = <fun>
|}]

let fst (type a : bits64) (t : a t) =
  match t with { fst; _ } -> fst
[%%expect{|
val fst : ('a : bits64). 'a t -> 'a = <fun>
|}]

let set_snd t a = t.snd <- a
[%%expect {|
val set_snd : 'a t -> 'a -> unit = <fun>
|}]

let set_snd t (a : int64#) = t.snd <- a
[%%expect {|
val set_snd : int64# t -> int64# -> unit = <fun>
|}]

let set_snd (type a : bits64) (t : a t) a = t.snd <- a
[%%expect {|
val set_snd : ('a : bits64). 'a t -> 'a -> unit = <fun>
|}]

let make fst snd = { fst; snd }
[%%expect{|
val make : 'a -> 'a -> 'a t = <fun>
|}]

let make (fst : int) snd = { fst; snd }
[%%expect{|
val make : int -> int -> int t = <fun>
|}]

let make (fst : int) (snd : int) = { fst; snd }
[%%expect{|
val make : int -> int -> int t = <fun>
|}]

let make fst snd : int t = { fst; snd }
[%%expect{|
val make : int -> int -> int t = <fun>
|}]

let make (type a : value) (fst : a) (snd : a) = { fst; snd }
[%%expect{|
val make : 'a -> 'a -> 'a t = <fun>
|}]

let make (type a : value) fst snd : a t = { fst; snd }
[%%expect{|
val make : 'a -> 'a -> 'a t = <fun>
|}]

let make (fst : int64#) snd = { fst; snd }
[%%expect{|
val make : int64# -> int64# -> int64# t = <fun>
|}]

let make (fst : int64#) (snd : int64#) = { fst; snd }
[%%expect{|
val make : int64# -> int64# -> int64# t = <fun>
|}]

let make (type a : bits64) (fst : a) (snd : a) = { fst; snd }
[%%expect{|
val make : ('a : bits64). 'a -> 'a -> 'a t = <fun>
|}]

external box_int64 : int64# -> int64 = "%box_int64"
[%%expect {|
external box_int64 : int64# -> int64 = "%box_int64"
|}]

let make_test_block () = { fst = #1L; snd = #2L } |> Sys.opaque_identity
[%%expect {|
val make_test_block : unit -> int64# t = <fun>
|}]

let test_block = make_test_block ()
[%%expect {|
val test_block : int64# t = <thing with field of kind any>
|}]

let test_direct =
  (* Check that projecting from a block with an [any] accounts for the layouts
     of all previous fields in the block *)
  test_block.snd |> box_int64
[%%expect{|
val test_direct : int64 = 2L
|}]

let test_via_index =
  (* Check that the same works when accessing through a block index *)
  let idx = ((.snd) : (('a : bits64) t, 'a) idx_mut) |> Sys.opaque_identity in
  Idx_mut.unsafe_get test_block idx |> box_int64
[%%expect {|
val test_via_index : int64 = 2L
|}]

let test_set_direct =
  let t = make_test_block () in
  t.snd <- #42L;
  (t |> Sys.opaque_identity).snd |> box_int64
[%%expect {|
val test_set_direct : int64 = 42L
|}]

let test_set_via_index =
  let t = make_test_block () in
  let idx = ((.snd) : (('a : bits64) t, 'a) idx_mut) |> Sys.opaque_identity in
  Idx_mut.unsafe_set t idx #42L;
  Idx_mut.unsafe_get t idx |> box_int64
[%%expect {|
val test_set_via_index : int64 = 42L
|}]

let test_unboxed_pair_block : #(int64# * int64#) t =
  { fst = #(#1L, #2L); snd = #(#3L, #4L) }
|> Sys.opaque_identity
[%%expect {|
val test_unboxed_pair_block : #(int64# * int64#) t =
  <thing with field of kind any>
|}]

let test_unboxed_pair_direct =
  let #(_fst, snd) = test_unboxed_pair_block.snd in snd |> box_int64
[%%expect{|
val test_unboxed_pair_direct : int64 = 4L
|}]

let test_nested_block : int64# t# t =
  { fst = #{ fst = #1L; snd = #2L }; snd = #{ fst = #3L; snd = #4L } }
|> Sys.opaque_identity
[%%expect {|
val test_nested_block : int64# t# t = <thing with field of kind any>
|}]

let test_nested_direct =
  test_nested_block.snd.#snd |> box_int64
[%%expect{|
val test_nested_direct : int64 = 4L
|}]

let test_nested_via_index =
  let idx =
    ((.snd.#snd) : (int64# t# t, int64#) idx_mut)
    |> Sys.opaque_identity
  in
  Idx_mut.unsafe_get test_nested_block idx |> box_int64
[%%expect{|
val test_nested_via_index : int64 = 4L
|}]
