(* TEST
   include stdlib_stable;
   flags = "-dlambda";
   expect;
*)

module Array = Stdlib.Array
open Stdlib_stable.Iarray

[%%expect {|
0
module Array = Array
0
|}]

(* Regression test showing that an [i]array of iarrays
   has element kind [addr].
 *)

let _ = [: [: :] :];;

[%%expect {|
(makearray_imm[addr] (makearray_imm[gen]))
- : 'a iarray iarray = [:[::]:]
|}]

let _ = [| [: :] |];;

[%%expect {|
(makearray[addr] (makearray_imm[gen]))
- : '_weak1 iarray array = [|[::]|]
|}]

(* Test that reading from an iarray generates an immutable load (iarray.get) *)

let arr = [: 1; 2; 3 :];;
[%%expect {|
(let (arr/411 =[value<intarray>] (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/411))
val arr : int iarray = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/411 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/411 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/411 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/411 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/411 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/411 1))
- : int = 2
|}]

type 'a alias = 'a iarray
let arr : int alias = [: 1; 2; 3 :];;
[%%expect {|
0
type 'a alias = 'a iarray
(let (arr/414 =[value<intarray>] (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/414))
val arr : int alias = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/414 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/414 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/414 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/414 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/414 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/414 1))
- : int = 2
|}]

let mut_arr = [| 1; 2; 3 |];;
let arr = of_array mut_arr;;
[%%expect {|
(let (mut_arr/415 =[value<intarray>] (makearray[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/415))
val mut_arr : int array = [|1; 2; 3|]
(let
  (mut_arr/415 =? (apply (field_imm 0 (global Toploop!)) "mut_arr")
   arr/416 =[value<intarray>]
     (apply (field_imm 13 (global Stdlib_stable__Iarray!)) mut_arr/415))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/416))
val arr : int iarray = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/416 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/416 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/416 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/416 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/416 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/416 1))
- : int = 2
|}]

type 'a alias = 'a iarray
let arr : int alias = [: 1; 2; 3 :];;
[%%expect {|
0
type 'a alias = 'a iarray
(let (arr/419 =[value<intarray>] (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/419))
val arr : int alias = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/419 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/419 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/419 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/419 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/419 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/419 1))
- : int = 2
|}]

let mut_arr = [| 1; 2; 3 |];;
let arr = of_array mut_arr;;
[%%expect {|
(let (mut_arr/420 =[value<intarray>] (makearray[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/420))
val mut_arr : int array = [|1; 2; 3|]
(let
  (mut_arr/420 =? (apply (field_imm 0 (global Toploop!)) "mut_arr")
   arr/421 =[value<intarray>]
     (apply (field_imm 13 (global Stdlib_stable__Iarray!)) mut_arr/420))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/421))
val arr : int iarray = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/421 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/421 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/421 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/421 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/421 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/421 1))
- : int = 2
|}]

type 'a alias = 'a iarray
let arr : int alias = [: 1; 2; 3 :];;
[%%expect {|
0
type 'a alias = 'a iarray
(let (arr/424 =[value<intarray>] (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/424))
val arr : int alias = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/424 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/424 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/424 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/424 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/424 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/424 1))
- : int = 2
|}]

let mut_arr = [| 1; 2; 3 |];;
let arr = of_array mut_arr;;
[%%expect {|
(let (mut_arr/425 =[value<intarray>] (makearray[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/425))
val mut_arr : int array = [|1; 2; 3|]
(let
  (mut_arr/425 =? (apply (field_imm 0 (global Toploop!)) "mut_arr")
   arr/426 =[value<intarray>]
     (apply (field_imm 13 (global Stdlib_stable__Iarray!)) mut_arr/425))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/426))
val arr : int iarray = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/426 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/426 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/426 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/426 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/426 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/426 1))
- : int = 2
|}]

(* And check that arrays are still mutable loads (array.get) *)

let mut_arr = [| 1; 2; 3 |];;
[%%expect {|
(let (mut_arr/427 =[value<intarray>] (makearray[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/427))
val mut_arr : int array = [|1; 2; 3|]
|}]

let _ = mut_arr.(1);;
[%%expect {|
(let (mut_arr/427 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/427 1))
- : int = 2
|}]

let _ = Array.get mut_arr 1;;
[%%expect {|
(let (mut_arr/427 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/427 1))
- : int = 2
|}]

let _ = Array.unsafe_get mut_arr 1;;
[%%expect {|
(let (mut_arr/427 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.unsafe_get[int indexed by int] mut_arr/427 1))
- : int = 2
|}]

type 'a alias = 'a array
let mut_arr : int alias = [| 1; 2; 3 |];;
[%%expect {|
0
type 'a alias = 'a array
(let (mut_arr/480 =[value<intarray>] (makearray[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/480))
val mut_arr : int alias = [|1; 2; 3|]
|}];;

let _ = mut_arr.(1);;
[%%expect {|
(let (mut_arr/480 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/480 1))
- : int = 2
|}]

let _ = Array.get mut_arr 1;;
[%%expect {|
(let (mut_arr/480 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/480 1))
- : int = 2
|}]

let _ = Array.unsafe_get mut_arr 1;;
[%%expect {|
(let (mut_arr/480 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.unsafe_get[int indexed by int] mut_arr/480 1))
- : int = 2
|}]

let arr = [: 1; 2; 3 :];;
let mut_arr = to_array arr;;
[%%expect {|
(let (arr/481 =[value<intarray>] (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/481))
val arr : int iarray = [:1; 2; 3:]
(let
  (arr/481 =? (apply (field_imm 0 (global Toploop!)) "arr")
   mut_arr/482 =[value<intarray>]
     (apply (field_imm 12 (global Stdlib_stable__Iarray!)) arr/481))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/482))
val mut_arr : int array = [|1; 2; 3|]
|}];;

let _ = mut_arr.(1);;
[%%expect {|
(let (mut_arr/482 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/482 1))
- : int = 2
|}]

let _ = Array.get mut_arr 1;;
[%%expect {|
(let (mut_arr/482 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/482 1))
- : int = 2
|}]

let _ = Array.unsafe_get mut_arr 1;;
[%%expect {|
(let (mut_arr/482 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.unsafe_get[int indexed by int] mut_arr/482 1))
- : int = 2
|}]
