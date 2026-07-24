(* TEST
 flags = "-extension simd_alpha -extension runtime_metaprogramming -extension small_numbers -extension layouts";
 expect;
*)

#syntax quotations on

(* This test file should be kept up to date with any [ident_*] in [predef.ml]. *)

module type S = sig type exn += Exit end
#mark_toplevel_in_quotations;;
[%%expect {|
module type S = sig type exn += Exit end
|}];;

(** Quoting **)

(* Type constructors *)

<[ fun (x : int) -> x ]>
[%%expect {|
- : <[int -> int]> expr = <[fun (x : int) -> x]>
|}];;
<[ fun (x : char) -> x ]>
[%%expect {|
- : <[char -> char]> expr = <[fun (x : char) -> x]>
|}];;
<[ fun (x : bytes) -> x ]>
[%%expect {|
- : <[bytes -> bytes]> expr = <[fun (x : bytes) -> x]>
|}];;
<[ fun (x : float) -> x ]>
[%%expect {|
- : <[float -> float]> expr = <[fun (x : float) -> x]>
|}];;
<[ fun (x : float32) -> x ]>
[%%expect {|
- : <[float32 -> float32]> expr = <[fun (x : float32) -> x]>
|}];;
<[ fun (x : bool) -> x ]>
[%%expect {|
- : <[bool -> bool]> expr = <[fun (x : bool) -> x]>
|}];;
<[ fun (x : unit) -> x ]>
[%%expect {|
- : <[unit -> unit]> expr = <[fun (x : unit) -> x]>
|}];;
<[ fun (x : exn) -> x ]>
[%%expect {|
- : <[exn -> exn]> expr = <[fun (x : exn) -> x]>
|}];;
<[ fun (x : _ eff) -> x ]>
[%%expect {|
- : <[$('a) eff -> $('a) eff]> expr = <[fun (x : _ eff) -> x]>
|}];;
<[ fun (x : _ continuation) -> x ]>
[%%expect {|
- : <[($('a), $('b)) continuation -> ($('a), $('b)) continuation]> expr =
<[fun (x : (_, _) continuation) -> x]>
|}];;
<[ fun (x : _ array) -> x ]>
[%%expect {|
- : <[$('a) array -> $('a) array]> expr = <[fun (x : _ array) -> x]>
|}];;
<[ fun (x : _ list) -> x ]>
[%%expect {|
- : <[$('a) list -> $('a) list]> expr = <[fun (x : _ list) -> x]>
|}];;
<[ fun (x : _ option) -> x ]>
[%%expect {|
- : <[$('a) option -> $('a) option]> expr = <[fun (x : _ option) -> x]>
|}];;
<[ fun (x : nativeint) -> x ]>
[%%expect {|
- : <[nativeint -> nativeint]> expr = <[fun (x : nativeint) -> x]>
|}];;
<[ fun (x : int8) -> x ]>
[%%expect {|
- : <[int8 -> int8]> expr = <[fun (x : int8) -> x]>
|}];;
<[ fun (x : int16) -> x ]>
[%%expect {|
- : <[int16 -> int16]> expr = <[fun (x : int16) -> x]>
|}];;
<[ fun (x : int32) -> x ]>
[%%expect {|
- : <[int32 -> int32]> expr = <[fun (x : int32) -> x]>
|}];;
<[ fun (x : int64) -> x ]>
[%%expect {|
- : <[int64 -> int64]> expr = <[fun (x : int64) -> x]>
|}];;
<[ fun (x : _ lazy_t) -> x ]>
[%%expect {|
- : <[$('a) lazy_t -> $('a) lazy_t]> expr = <[fun (x : _ lazy_t) -> x]>
|}];;
<[ fun (x : string) -> x ]>
[%%expect {|
- : <[string -> string]> expr = <[fun (x : string) -> x]>
|}];;
<[ fun (x : extension_constructor) -> x ]>
[%%expect {|
- : <[extension_constructor -> extension_constructor]> expr =
<[fun (x : extension_constructor) -> x]>
|}];;
<[ fun (x : floatarray) -> x ]>
[%%expect {|
- : <[floatarray -> floatarray]> expr = <[fun (x : floatarray) -> x]>
|}];;
<[ fun (x : _ iarray) -> x ]>
[%%expect {|
- : <[$('a) iarray -> $('a) iarray]> expr = <[fun (x : _ iarray) -> x]>
|}];;
<[ fun (x : _ atomic_loc) -> x ]>
[%%expect {|
- : <[$('a) atomic_loc -> $('a) atomic_loc]> expr =
<[fun (x : _ atomic_loc) -> x]>
|}];;
<[ fun (x : lexing_position) -> x ]>
[%%expect {|
- : <[lexing_position -> lexing_position]> expr =
<[fun (x : lexing_position) -> x]>
|}];;
<[ fun (x : _ expr) -> x ]>
[%%expect {|
- : <[$('a) expr -> $('a) expr]> expr = <[fun (x : _ expr) -> x]>
|}];;
<[ fun (x : _ eval) -> x ]>
[%%expect {|
- : <[$('a) eval -> $('a) eval]> expr = <[fun (x : _ eval) -> x]>
|}];;
<[ fun (x : _ box) -> x ]>
[%%expect {|
- : <[$('a) box -> $('a) box]> expr = <[fun (x : _ box) -> x]>
|}];;
<[ fun (x : nativeint_u) -> x ]>
[%%expect {|
- : <[nativeint_u -> nativeint_u]> expr = <[fun (x : nativeint_u) -> x]>
|}];;
<[ fun (x : int32_u) -> x ]>
[%%expect {|
- : <[int32_u -> int32_u]> expr = <[fun (x : int32_u) -> x]>
|}];;
<[ fun (x : int64_u) -> x ]>
[%%expect {|
- : <[int64_u -> int64_u]> expr = <[fun (x : int64_u) -> x]>
|}];;
<[ fun (x : float32_u) -> x ]>
[%%expect {|
- : <[float32_u -> float32_u]> expr = <[fun (x : float32_u) -> x]>
|}];;
<[ fun (x : uint8_u) -> x ]>
[%%expect {|
- : <[uint8_u -> uint8_u]> expr = <[fun (x : uint8_u) -> x]>
|}];;
<[ fun (x : uint16_u) -> x ]>
[%%expect {|
- : <[uint16_u -> uint16_u]> expr = <[fun (x : uint16_u) -> x]>
|}];;
<[ fun (x : uint32_u) -> x ]>
[%%expect {|
- : <[uint32_u -> uint32_u]> expr = <[fun (x : uint32_u) -> x]>
|}];;
<[ fun (x : uint64_u) -> x ]>
[%%expect {|
- : <[uint64_u -> uint64_u]> expr = <[fun (x : uint64_u) -> x]>
|}];;
<[ fun (x : _ or_null) -> x ]>
[%%expect {|
- : <[$('a) or_null -> $('a) or_null]> expr = <[fun (x : _ or_null) -> x]>
|}];;
<[ fun (x : _ idx_imm) -> x ]>
[%%expect {|
- : <[($('a), $('b)) idx_imm -> ($('a), $('b)) idx_imm]> expr =
<[fun (x : (_, _) idx_imm) -> x]>
|}];;
<[ fun (x : _ idx_mut) -> x ]>
[%%expect {|
- : <[($('a), $('b)) idx_mut -> ($('a), $('b)) idx_mut]> expr =
<[fun (x : (_, _) idx_mut) -> x]>
|}];;
<[ fun (x : int8x16) -> x ]>
[%%expect {|
- : <[int8x16 -> int8x16]> expr = <[fun (x : int8x16) -> x]>
|}];;
<[ fun (x : int16x8) -> x ]>
[%%expect {|
- : <[int16x8 -> int16x8]> expr = <[fun (x : int16x8) -> x]>
|}];;
<[ fun (x : int32x4) -> x ]>
[%%expect {|
- : <[int32x4 -> int32x4]> expr = <[fun (x : int32x4) -> x]>
|}];;
<[ fun (x : int64x2) -> x ]>
[%%expect {|
- : <[int64x2 -> int64x2]> expr = <[fun (x : int64x2) -> x]>
|}];;
<[ fun (x : float16x8) -> x ]>
[%%expect {|
- : <[float16x8 -> float16x8]> expr = <[fun (x : float16x8) -> x]>
|}];;
<[ fun (x : float32x4) -> x ]>
[%%expect {|
- : <[float32x4 -> float32x4]> expr = <[fun (x : float32x4) -> x]>
|}];;
<[ fun (x : float64x2) -> x ]>
[%%expect {|
- : <[float64x2 -> float64x2]> expr = <[fun (x : float64x2) -> x]>
|}];;
<[ fun (x : int8x32) -> x ]>
[%%expect {|
- : <[int8x32 -> int8x32]> expr = <[fun (x : int8x32) -> x]>
|}];;
<[ fun (x : int16x16) -> x ]>
[%%expect {|
- : <[int16x16 -> int16x16]> expr = <[fun (x : int16x16) -> x]>
|}];;
<[ fun (x : int32x8) -> x ]>
[%%expect {|
- : <[int32x8 -> int32x8]> expr = <[fun (x : int32x8) -> x]>
|}];;
<[ fun (x : int64x4) -> x ]>
[%%expect {|
- : <[int64x4 -> int64x4]> expr = <[fun (x : int64x4) -> x]>
|}];;
<[ fun (x : float16x16) -> x ]>
[%%expect {|
- : <[float16x16 -> float16x16]> expr = <[fun (x : float16x16) -> x]>
|}];;
<[ fun (x : float32x8) -> x ]>
[%%expect {|
- : <[float32x8 -> float32x8]> expr = <[fun (x : float32x8) -> x]>
|}];;
<[ fun (x : float64x4) -> x ]>
[%%expect {|
- : <[float64x4 -> float64x4]> expr = <[fun (x : float64x4) -> x]>
|}];;
<[ fun (x : int8x64) -> x ]>
[%%expect {|
- : <[int8x64 -> int8x64]> expr = <[fun (x : int8x64) -> x]>
|}];;
<[ fun (x : int16x32) -> x ]>
[%%expect {|
- : <[int16x32 -> int16x32]> expr = <[fun (x : int16x32) -> x]>
|}];;
<[ fun (x : int32x16) -> x ]>
[%%expect {|
- : <[int32x16 -> int32x16]> expr = <[fun (x : int32x16) -> x]>
|}];;
<[ fun (x : int64x8) -> x ]>
[%%expect {|
- : <[int64x8 -> int64x8]> expr = <[fun (x : int64x8) -> x]>
|}];;
<[ fun (x : float16x32) -> x ]>
[%%expect {|
- : <[float16x32 -> float16x32]> expr = <[fun (x : float16x32) -> x]>
|}];;
<[ fun (x : float32x16) -> x ]>
[%%expect {|
- : <[float32x16 -> float32x16]> expr = <[fun (x : float32x16) -> x]>
|}];;
<[ fun (x : float64x8) -> x ]>
[%%expect {|
- : <[float64x8 -> float64x8]> expr = <[fun (x : float64x8) -> x]>
|}];;

<[ fun (x : my_type) -> x ]> (* sanity check *)
[%%expect {|
Line 1, characters 12-19:
1 | <[ fun (x : my_type) -> x ]> (* sanity check *)
                ^^^^^^^
Error: Unbound type constructor "my_type"
|}];;

(* Exceptions *)

<[ raise (Match_failure ("", 0, 0)) ]>
[%%expect {|
- : 'a expr = <[Stdlib.raise (Stdlib.Match_failure ("", 0, 0))]>
|}];;
<[ raise Out_of_memory ]>
[%%expect {|
- : 'a expr = <[Stdlib.raise Stdlib.Out_of_memory]>
|}];;
<[ raise Out_of_fibers ]>
[%%expect {|
- : 'a expr = <[Stdlib.raise Out_of_fibers]>
|}];;
<[ raise (Invalid_argument "") ]>
[%%expect {|
- : 'a expr = <[Stdlib.raise (Stdlib.Invalid_argument "")]>
|}];;
<[ raise (Failure "") ]>
[%%expect {|
- : 'a expr = <[Stdlib.raise (Stdlib.Failure "")]>
|}];;
<[ raise Not_found ]>
[%%expect {|
- : 'a expr = <[Stdlib.raise Stdlib.Not_found]>
|}];;
<[ raise (Sys_error "") ]>
[%%expect {|
- : 'a expr = <[Stdlib.raise (Stdlib.Sys_error "")]>
|}];;
<[ raise End_of_file ]>
[%%expect {|
- : 'a expr = <[Stdlib.raise Stdlib.End_of_file]>
|}];;
<[ raise Division_by_zero ]>
[%%expect {|
- : 'a expr = <[Stdlib.raise Stdlib.Division_by_zero]>
|}];;
<[ raise Stack_overflow ]>
[%%expect {|
- : 'a expr = <[Stdlib.raise Stdlib.Stack_overflow]>
|}];;
<[ raise Sys_blocked_io ]>
[%%expect {|
- : 'a expr = <[Stdlib.raise Stdlib.Sys_blocked_io]>
|}];;
<[ raise (Assert_failure ("", 0, 0)) ]>
[%%expect {|
- : 'a expr = <[Stdlib.raise (Stdlib.Assert_failure ("", 0, 0))]>
|}];;
<[ raise (Undefined_recursive_module ("", 0, 0)) ]>
[%%expect {|
- : 'a expr = <[Stdlib.raise (Stdlib.Undefined_recursive_module ("", 0, 0))]>
|}];;
<[ raise Continuation_already_taken ]>
[%%expect {|
- : 'a expr = <[Stdlib.raise Continuation_already_taken]>
|}];;

<[ raise My_exception ]> (* sanity check *)
[%%expect {|
Line 1, characters 9-21:
1 | <[ raise My_exception ]> (* sanity check *)
             ^^^^^^^^^^^^
Error: This variant expression is expected to have type "exn"
       There is no constructor "My_exception" within type "exn"
|}];;

(* Constructors *)

<[ false ]>
[%%expect {|
- : <[bool]> expr = <[false]>
|}];;
<[ true ]>
[%%expect {|
- : <[bool]> expr = <[true]>
|}];;
<[ () ]>
[%%expect {|
- : <[unit]> expr = <[()]>
|}];;
<[ [] ]>
[%%expect {|
- : <[$('a) list]> expr = <[[]]>
|}];;
<[ (() :: []) ]>
[%%expect {|
- : <[unit list]> expr = <[[()]]>
|}];;
<[ None ]>
[%%expect {|
- : <[$('a) option]> expr = <[None]>
|}];;
<[ Some () ]>
[%%expect {|
- : <[unit option]> expr = <[Some ()]>
|}];;
<[ Null ]>
[%%expect {|
- : <[$('a) or_null]> expr = <[Null]>
|}];;
<[ This () ]>
[%%expect {|
- : <[unit or_null]> expr = <[This ()]>
|}];;

<[ My_constructor () ]> (* sanity check *)
[%%expect {|
Line 1, characters 3-17:
1 | <[ My_constructor () ]> (* sanity check *)
       ^^^^^^^^^^^^^^
Error: Unbound constructor "My_constructor"
|}];;

(** Shadowing builtins in quotes **)

(* Type *)
(* CR jbachurski: This is wrong -- the [int] in [e] refers to prelude,
   not the local one.  *)
let e = <[ fun (x : int) -> x ]> in <[ fun (type int) () -> $e ]>
[%%expect {|
- : <[unit -> int -> int]> expr = <[fun (type int) () -> fun (x : int) -> x]>
|}];;
(* Exception *)
let e = <[ raise Exit ]> in <[ let exception Exit in $e ]>
[%%expect {|
- : 'a expr = <[let exception Exit in Stdlib.raise Stdlib.Exit]>
|}];;
(* We can avoid the following two bugs by banning [let exception],
   but they seem unlikely enough. *)
(* CR jbachurski: This is wrong -- the inner [Exit] should refer to the
   global [Stdlib], not the argument. *)
<[ fun (module Stdlib : S) -> raise Exit ]>
[%%expect {|
- : <[(module S) -> $('a)]> expr =
<[
  fun (((module Stdlib) : (module S)) : (module S)) ->
    Stdlib.raise Stdlib.Exit
]>
|}];;
(* CR jbachurski: This is wrong -- the inner exn should refer to the
   global [Stdlib], not the extension. *)
let e = <[ raise Continuation_already_taken ]> in <[ let exception Continuation_already_taken in $e ]>
[%%expect {|
- : 'a expr =
<[
  let exception Continuation_already_taken in
    Stdlib.raise Continuation_already_taken
]>
|}];;

(* Similar examples to the above could be constructed for locally declared
   types if we had struct syntax in quotes. *)
