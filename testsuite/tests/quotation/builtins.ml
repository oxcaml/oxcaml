(* TEST
 flags = "-extension simd_alpha -extension runtime_metaprogramming -extension small_numbers -extension layouts";
 expect;
*)

#syntax quotations on

(* This test file should be kept up to date with any [ident_*] in [predef.ml]. *)

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
>> Fatal error: Translquote [at Line 1, characters 12-17]: cannot quote type eff - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

|}];;
<[ fun (x : _ continuation) -> x ]>
[%%expect {|
>> Fatal error: Translquote [at Line 1, characters 12-26]: cannot quote type continuation - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

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
>> Fatal error: Translquote [at Line 1, characters 12-16]: cannot quote type int8 - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

|}];;
<[ fun (x : int16) -> x ]>
[%%expect {|
>> Fatal error: Translquote [at Line 1, characters 12-17]: cannot quote type int16 - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

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
>> Fatal error: Translquote [at Line 1, characters 12-24]: cannot quote type atomic_loc - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

|}];;
<[ fun (x : lexing_position) -> x ]>
[%%expect {|
- : <[lexing_position -> lexing_position]> expr =
<[fun (x : lexing_position) -> x]>
|}];;
<[ fun (x : _ code) -> x ]>
[%%expect {|
Line 1, characters 14-18:
1 | <[ fun (x : _ code) -> x ]>
                  ^^^^
Error: Unbound type constructor "code"
|}];;
<[ fun (x : _ eval) -> x ]>
[%%expect {|
- : <[$('a) eval -> $('a) eval]> expr = <[fun (x : _ eval) -> x]>
|}];;
<[ fun (x : _ box) -> x ]>
[%%expect {|
>> Fatal error: Translquote [at Line 1, characters 12-17]: cannot quote type box - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

|}];;
<[ fun (x : nativeint_u) -> x ]>
[%%expect {|
- : <[nativeint_u -> nativeint_u]> expr = <[fun (x : nativeint#) -> x]>
|}];;
<[ fun (x : int32_u) -> x ]>
[%%expect {|
- : <[int32_u -> int32_u]> expr = <[fun (x : int32#) -> x]>
|}];;
<[ fun (x : int64_u) -> x ]>
[%%expect {|
- : <[int64_u -> int64_u]> expr = <[fun (x : int64#) -> x]>
|}];;
<[ fun (x : float32_u) -> x ]>
[%%expect {|
>> Fatal error: Translquote [at Line 1, characters 12-21]: cannot quote type float32_u - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

|}];;
<[ fun (x : uint8_u) -> x ]>
[%%expect {|
>> Fatal error: Translquote [at Line 1, characters 12-19]: cannot quote type uint8_u - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

|}];;
<[ fun (x : uint16_u) -> x ]>
[%%expect {|
>> Fatal error: Translquote [at Line 1, characters 12-20]: cannot quote type uint16_u - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

|}];;
<[ fun (x : uint32_u) -> x ]>
[%%expect {|
>> Fatal error: Translquote [at Line 1, characters 12-20]: cannot quote type uint32_u - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

|}];;
<[ fun (x : uint64_u) -> x ]>
[%%expect {|
>> Fatal error: Translquote [at Line 1, characters 12-20]: cannot quote type uint64_u - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

|}];;
<[ fun (x : _ or_null) -> x ]>
[%%expect {|
>> Fatal error: Translquote [at Line 1, characters 12-21]: cannot quote type or_null - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

|}];;
<[ fun (x : _ idx_imm) -> x ]>
[%%expect {|
>> Fatal error: Translquote [at Line 1, characters 12-21]: cannot quote type idx_imm - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

|}];;
<[ fun (x : _ idx_mut) -> x ]>
[%%expect {|
>> Fatal error: Translquote [at Line 1, characters 12-21]: cannot quote type idx_mut - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

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
>> Fatal error: Translquote [at Line 1, characters 12-21]: cannot quote type float16x8 - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

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
>> Fatal error: Translquote [at Line 1, characters 12-19]: cannot quote type int8x32 - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

|}];;
<[ fun (x : int16x16) -> x ]>
[%%expect {|
>> Fatal error: Translquote [at Line 1, characters 12-20]: cannot quote type int16x16 - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

|}];;
<[ fun (x : int32x8) -> x ]>
[%%expect {|
>> Fatal error: Translquote [at Line 1, characters 12-19]: cannot quote type int32x8 - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

|}];;
<[ fun (x : int64x4) -> x ]>
[%%expect {|
>> Fatal error: Translquote [at Line 1, characters 12-19]: cannot quote type int64x4 - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

|}];;
<[ fun (x : float16x16) -> x ]>
[%%expect {|
>> Fatal error: Translquote [at Line 1, characters 12-22]: cannot quote type float16x16 - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

|}];;
<[ fun (x : float32x8) -> x ]>
[%%expect {|
>> Fatal error: Translquote [at Line 1, characters 12-21]: cannot quote type float32x8 - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

|}];;
<[ fun (x : float64x4) -> x ]>
[%%expect {|
>> Fatal error: Translquote [at Line 1, characters 12-21]: cannot quote type float64x4 - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

|}];;
<[ fun (x : int8x64) -> x ]>
[%%expect {|
>> Fatal error: Translquote [at Line 1, characters 12-19]: cannot quote type int8x64 - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

|}];;
<[ fun (x : int16x32) -> x ]>
[%%expect {|
>> Fatal error: Translquote [at Line 1, characters 12-20]: cannot quote type int16x32 - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

|}];;
<[ fun (x : int32x16) -> x ]>
[%%expect {|
>> Fatal error: Translquote [at Line 1, characters 12-20]: cannot quote type int32x16 - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

|}];;
<[ fun (x : int64x8) -> x ]>
[%%expect {|
>> Fatal error: Translquote [at Line 1, characters 12-19]: cannot quote type int64x8 - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

|}];;
<[ fun (x : float16x32) -> x ]>
[%%expect {|
>> Fatal error: Translquote [at Line 1, characters 12-22]: cannot quote type float16x32 - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

|}];;
<[ fun (x : float32x16) -> x ]>
[%%expect {|
>> Fatal error: Translquote [at Line 1, characters 12-22]: cannot quote type float32x16 - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

|}];;
<[ fun (x : float64x8) -> x ]>
[%%expect {|
>> Fatal error: Translquote [at Line 1, characters 12-21]: cannot quote type float64x8 - this is either unsupported or a bug
Uncaught exception: Misc.Fatal_error

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
