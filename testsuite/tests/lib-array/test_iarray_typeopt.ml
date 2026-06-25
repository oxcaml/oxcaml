(* TEST
   include stdlib_stable;
   flags = "-dlambda -dcanonical-ids";
   expect;
*)

module Array = Stdlib.Array
open Stdlib_stable.IarrayLabels

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
(let (arr/0 =[value<intarray>] (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/0))
val arr : int iarray = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/0 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/0 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/0 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/0 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/0 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/0 1))
- : int = 2
|}]

type 'a alias = 'a iarray
let arr : int alias = [: 1; 2; 3 :];;
[%%expect {|
0
type 'a alias = 'a iarray
(let (arr/1 =[value<intarray>] (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/1))
val arr : int alias = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/1 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/1 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/1 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/1 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/1 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/1 1))
- : int = 2
|}]

let mut_arr = [| 1; 2; 3 |];;
let arr = of_array mut_arr;;
[%%expect {|
(let (mut_arr/0 =[value<intarray>] (makearray[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/0))
val mut_arr : int array = [|1; 2; 3|]
(let
  (mut_arr/0 =? (apply (field_imm 0 (global Toploop!)) "mut_arr")
   arr/2 =[value<intarray>]
     (apply (field_imm 13 (global Stdlib_stable__IarrayLabels!)) mut_arr/0))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/2))
val arr : int iarray = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/2 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/2 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/2 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/2 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/2 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/2 1))
- : int = 2
|}]

type 'a alias = 'a iarray
let arr : int alias = [: 1; 2; 3 :];;
[%%expect {|
0
type 'a alias = 'a iarray
(let (arr/3 =[value<intarray>] (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/3))
val arr : int alias = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/3 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/3 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/3 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/3 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/3 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/3 1))
- : int = 2
|}]

let mut_arr = [| 1; 2; 3 |];;
let arr = of_array mut_arr;;
[%%expect {|
(let (mut_arr/1 =[value<intarray>] (makearray[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/1))
val mut_arr : int array = [|1; 2; 3|]
(let
  (mut_arr/1 =? (apply (field_imm 0 (global Toploop!)) "mut_arr")
   arr/4 =[value<intarray>]
     (apply (field_imm 13 (global Stdlib_stable__IarrayLabels!)) mut_arr/1))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/4))
val arr : int iarray = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/4 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/4 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/4 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/4 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/4 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/4 1))
- : int = 2
|}]

type 'a alias = 'a iarray
let arr : int alias = [: 1; 2; 3 :];;
[%%expect {|
0
type 'a alias = 'a iarray
(let (arr/5 =[value<intarray>] (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/5))
val arr : int alias = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/5 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/5 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/5 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/5 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/5 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/5 1))
- : int = 2
|}]

let mut_arr = [| 1; 2; 3 |];;
let arr = of_array mut_arr;;
[%%expect {|
(let (mut_arr/2 =[value<intarray>] (makearray[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/2))
val mut_arr : int array = [|1; 2; 3|]
(let
  (mut_arr/2 =? (apply (field_imm 0 (global Toploop!)) "mut_arr")
   arr/6 =[value<intarray>]
     (apply (field_imm 13 (global Stdlib_stable__IarrayLabels!)) mut_arr/2))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/6))
val arr : int iarray = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/6 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/6 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/6 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/6 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/6 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/6 1))
- : int = 2
|}]

type 'a alias = 'a iarray
let arr : int alias = [: 1; 2; 3 :];;
[%%expect {|
0
type 'a alias = 'a iarray
(let (arr/7 =[value<intarray>] (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/7))
val arr : int alias = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/7 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/7 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/7 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/7 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/7 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/7 1))
- : int = 2
|}]

let mut_arr = [| 1; 2; 3 |];;
let arr = of_array mut_arr;;
[%%expect {|
(let (mut_arr/3 =[value<intarray>] (makearray[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/3))
val mut_arr : int array = [|1; 2; 3|]
(let
  (mut_arr/3 =? (apply (field_imm 0 (global Toploop!)) "mut_arr")
   arr/8 =[value<intarray>]
     (apply (field_imm 13 (global Stdlib_stable__IarrayLabels!)) mut_arr/3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/8))
val arr : int iarray = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/8 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/8 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/8 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/8 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/8 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/8 1))
- : int = 2
|}]

type 'a alias = 'a iarray
let arr : int alias = [: 1; 2; 3 :];;
[%%expect {|
0
type 'a alias = 'a iarray
(let (arr/9 =[value<intarray>] (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/9))
val arr : int alias = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/9 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/9 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/9 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/9 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/9 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/9 1))
- : int = 2
|}]

let mut_arr = [| 1; 2; 3 |];;
let arr = of_array mut_arr;;
[%%expect {|
(let (mut_arr/4 =[value<intarray>] (makearray[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/4))
val mut_arr : int array = [|1; 2; 3|]
(let
  (mut_arr/4 =? (apply (field_imm 0 (global Toploop!)) "mut_arr")
   arr/10 =[value<intarray>]
     (apply (field_imm 13 (global Stdlib_stable__IarrayLabels!)) mut_arr/4))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/10))
val arr : int iarray = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/10 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/10 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/10 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/10 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/10 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/10 1))
- : int = 2
|}]

(* And check that arrays are still mutable loads (array.get) *)

let mut_arr = [| 1; 2; 3 |];;
[%%expect {|
(let (mut_arr/5 =[value<intarray>] (makearray[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/5))
val mut_arr : int array = [|1; 2; 3|]
|}]

let _ = mut_arr.(1);;
[%%expect {|
(let (mut_arr/5 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/5 1))
- : int = 2
|}]

let _ = Array.get mut_arr 1;;
[%%expect {|
(let (mut_arr/5 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/5 1))
- : int = 2
|}]

let _ = Array.unsafe_get mut_arr 1;;
[%%expect {|
(let (mut_arr/5 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.unsafe_get[int indexed by int] mut_arr/5 1))
- : int = 2
|}]

type 'a alias = 'a array
let mut_arr : int alias = [| 1; 2; 3 |];;
[%%expect {|
0
type 'a alias = 'a array
(let (mut_arr/6 =[value<intarray>] (makearray[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/6))
val mut_arr : int alias = [|1; 2; 3|]
|}];;

let _ = mut_arr.(1);;
[%%expect {|
(let (mut_arr/6 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/6 1))
- : int = 2
|}]

let _ = Array.get mut_arr 1;;
[%%expect {|
(let (mut_arr/6 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/6 1))
- : int = 2
|}]

let _ = Array.unsafe_get mut_arr 1;;
[%%expect {|
(let (mut_arr/6 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.unsafe_get[int indexed by int] mut_arr/6 1))
- : int = 2
|}]

let arr = [: 1; 2; 3 :];;
let mut_arr = to_array arr;;
[%%expect {|
(let (arr/11 =[value<intarray>] (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/11))
val arr : int iarray = [:1; 2; 3:]
(let
  (arr/11 =? (apply (field_imm 0 (global Toploop!)) "arr")
   mut_arr/7 =[value<intarray>]
     (apply (field_imm 12 (global Stdlib_stable__IarrayLabels!)) arr/11))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/7))
val mut_arr : int array = [|1; 2; 3|]
|}];;

let _ = mut_arr.(1);;
[%%expect {|
(let (mut_arr/7 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/7 1))
- : int = 2
|}]

let _ = Array.get mut_arr 1;;
[%%expect {|
(let (mut_arr/7 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/7 1))
- : int = 2
|}]

let _ = Array.unsafe_get mut_arr 1;;
[%%expect {|
(let (mut_arr/7 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.unsafe_get[int indexed by int] mut_arr/7 1))
- : int = 2
|}]
