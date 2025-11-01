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
(let (arr/400 =[value<extarray>] (makearray_imm[ext] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/400))
val arr : int iarray = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/400 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[ext indexed by int] arr/400 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/400 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[ext indexed by int] arr/400 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/400 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[ext indexed by int] arr/400 1))
- : int = 2
|}]

type 'a alias = 'a iarray
let arr : int alias = [: 1; 2; 3 :];;
[%%expect {|
0
type 'a alias = 'a iarray
(let (arr/402 =[value<extarray>] (makearray_imm[ext] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/402))
val arr : int alias = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/402 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[ext indexed by int] arr/402 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/402 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[ext indexed by int] arr/402 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/402 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[ext indexed by int] arr/402 1))
- : int = 2
|}]

let mut_arr = [| 1; 2; 3 |];;
let arr = of_array mut_arr;;
[%%expect {|
(let (mut_arr/403 =[value<extarray>] (makearray[ext] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/403))
val mut_arr : int array = [|1; 2; 3|]
(let
  (mut_arr/403 =? (apply (field_imm 0 (global Toploop!)) "mut_arr")
   arr/404 =[value<extarray>]
     (apply (field_imm 13 (global Stdlib_stable__Iarray!)) mut_arr/403))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/404))
val arr : int iarray = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/404 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[ext indexed by int] arr/404 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/404 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[ext indexed by int] arr/404 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/404 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[ext indexed by int] arr/404 1))
- : int = 2
|}]

type 'a alias = 'a iarray
let arr : int alias = [: 1; 2; 3 :];;
[%%expect {|
0
type 'a alias = 'a iarray
(let (arr/406 =[value<extarray>] (makearray_imm[ext] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/406))
val arr : int alias = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/406 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[ext indexed by int] arr/406 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/406 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[ext indexed by int] arr/406 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/406 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[ext indexed by int] arr/406 1))
- : int = 2
|}]

let mut_arr = [| 1; 2; 3 |];;
let arr = of_array mut_arr;;
[%%expect {|
(let (mut_arr/407 =[value<extarray>] (makearray[ext] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/407))
val mut_arr : int array = [|1; 2; 3|]
(let
  (mut_arr/407 =? (apply (field_imm 0 (global Toploop!)) "mut_arr")
   arr/408 =[value<extarray>]
     (apply (field_imm 13 (global Stdlib_stable__Iarray!)) mut_arr/407))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/408))
val arr : int iarray = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/408 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[ext indexed by int] arr/408 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/408 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[ext indexed by int] arr/408 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/408 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[ext indexed by int] arr/408 1))
- : int = 2
|}]

type 'a alias = 'a iarray
let arr : int alias = [: 1; 2; 3 :];;
[%%expect {|
0
type 'a alias = 'a iarray
(let (arr/410 =[value<extarray>] (makearray_imm[ext] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/410))
val arr : int alias = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/410 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[ext indexed by int] arr/410 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/410 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[ext indexed by int] arr/410 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/410 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[ext indexed by int] arr/410 1))
- : int = 2
|}]

let mut_arr = [| 1; 2; 3 |];;
let arr = of_array mut_arr;;
[%%expect {|
(let (mut_arr/411 =[value<extarray>] (makearray[ext] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/411))
val mut_arr : int array = [|1; 2; 3|]
(let
  (mut_arr/411 =? (apply (field_imm 0 (global Toploop!)) "mut_arr")
   arr/412 =[value<extarray>]
     (apply (field_imm 13 (global Stdlib_stable__Iarray!)) mut_arr/411))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/412))
val arr : int iarray = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/412 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[ext indexed by int] arr/412 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/412 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[ext indexed by int] arr/412 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/412 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[ext indexed by int] arr/412 1))
- : int = 2
|}]

(* And check that arrays are still mutable loads (array.get) *)

let mut_arr = [| 1; 2; 3 |];;
[%%expect {|
(let (mut_arr/413 =[value<extarray>] (makearray[ext] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/413))
val mut_arr : int array = [|1; 2; 3|]
|}]

let _ = mut_arr.(1);;
[%%expect {|
(let (mut_arr/413 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[ext indexed by int] mut_arr/413 1))
- : int = 2
|}]

let _ = Array.get mut_arr 1;;
[%%expect {|
(let (mut_arr/413 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[ext indexed by int] mut_arr/413 1))
- : int = 2
|}]

let _ = Array.unsafe_get mut_arr 1;;
[%%expect {|
(let (mut_arr/413 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.unsafe_get[ext indexed by int] mut_arr/413 1))
- : int = 2
|}]

type 'a alias = 'a array
let mut_arr : int alias = [| 1; 2; 3 |];;
[%%expect {|
0
type 'a alias = 'a array
(let (mut_arr/465 =[value<extarray>] (makearray[ext] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/465))
val mut_arr : int alias = [|1; 2; 3|]
|}];;

let _ = mut_arr.(1);;
[%%expect {|
(let (mut_arr/465 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[ext indexed by int] mut_arr/465 1))
- : int = 2
|}]

let _ = Array.get mut_arr 1;;
[%%expect {|
(let (mut_arr/465 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[ext indexed by int] mut_arr/465 1))
- : int = 2
|}]

let _ = Array.unsafe_get mut_arr 1;;
[%%expect {|
(let (mut_arr/465 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.unsafe_get[ext indexed by int] mut_arr/465 1))
- : int = 2
|}]

let arr = [: 1; 2; 3 :];;
let mut_arr = to_array arr;;
[%%expect {|
(let (arr/466 =[value<extarray>] (makearray_imm[ext] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/466))
val arr : int iarray = [:1; 2; 3:]
(let
  (arr/466 =? (apply (field_imm 0 (global Toploop!)) "arr")
   mut_arr/467 =[value<extarray>]
     (apply (field_imm 12 (global Stdlib_stable__Iarray!)) arr/466))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/467))
val mut_arr : int array = [|1; 2; 3|]
|}];;

let _ = mut_arr.(1);;
[%%expect {|
(let (mut_arr/467 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[ext indexed by int] mut_arr/467 1))
- : int = 2
|}]

let _ = Array.get mut_arr 1;;
[%%expect {|
(let (mut_arr/467 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[ext indexed by int] mut_arr/467 1))
- : int = 2
|}]

let _ = Array.unsafe_get mut_arr 1;;
[%%expect {|
(let (mut_arr/467 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.unsafe_get[ext indexed by int] mut_arr/467 1))
- : int = 2
|}]
