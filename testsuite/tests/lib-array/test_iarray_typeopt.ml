(* TEST
   include stdlib_stable;
   flags = "-dlambda";
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
(let (arr/404 =[value<intarray>] (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/404))
val arr : int iarray = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/404 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/404 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/404 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/404 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/404 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/404 1))
- : int = 2
|}]

type 'a alias = 'a iarray
let arr : int alias = [: 1; 2; 3 :];;
[%%expect {|
0
type 'a alias = 'a iarray
(let (arr/406 =[value<intarray>] (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/406))
val arr : int alias = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/406 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/406 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/406 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/406 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/406 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/406 1))
- : int = 2
|}]

let mut_arr = [| 1; 2; 3 |];;
let arr = of_array mut_arr;;
[%%expect {|
(let (mut_arr/407 =[value<intarray>] (makearray[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/407))
val mut_arr : int array = [|1; 2; 3|]
(let
  (mut_arr/407 =? (apply (field_imm 0 (global Toploop!)) "mut_arr")
   arr/408 =[value<intarray>]
     (apply (field_imm 13 (global Stdlib_stable__IarrayLabels!)) mut_arr/407))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/408))
val arr : int iarray = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/408 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/408 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/408 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/408 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/408 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/408 1))
- : int = 2
|}]

type 'a alias = 'a iarray
let arr : int alias = [: 1; 2; 3 :];;
[%%expect {|
0
type 'a alias = 'a iarray
(let (arr/410 =[value<intarray>] (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/410))
val arr : int alias = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/410 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/410 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/410 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/410 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/410 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/410 1))
- : int = 2
|}]

let mut_arr = [| 1; 2; 3 |];;
let arr = of_array mut_arr;;
[%%expect {|
(let (mut_arr/411 =[value<intarray>] (makearray[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/411))
val mut_arr : int array = [|1; 2; 3|]
(let
  (mut_arr/411 =? (apply (field_imm 0 (global Toploop!)) "mut_arr")
   arr/412 =[value<intarray>]
     (apply (field_imm 13 (global Stdlib_stable__IarrayLabels!)) mut_arr/411))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/412))
val arr : int iarray = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/412 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/412 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/412 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/412 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/412 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/412 1))
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
     (apply (field_imm 13 (global Stdlib_stable__IarrayLabels!)) mut_arr/415))
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

(* And check that arrays are still mutable loads (array.get) *)

let mut_arr = [| 1; 2; 3 |];;
[%%expect {|
(let (mut_arr/417 =[value<intarray>] (makearray[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/417))
val mut_arr : int array = [|1; 2; 3|]
|}]

let _ = mut_arr.(1);;
[%%expect {|
(let (mut_arr/417 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/417 1))
- : int = 2
|}]

let _ = Array.get mut_arr 1;;
[%%expect {|
(let (mut_arr/417 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/417 1))
- : int = 2
|}]

let _ = Array.unsafe_get mut_arr 1;;
[%%expect {|
(let (mut_arr/417 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.unsafe_get[int indexed by int] mut_arr/417 1))
- : int = 2
|}]

type 'a alias = 'a array
let mut_arr : int alias = [| 1; 2; 3 |];;
[%%expect {|
0
type 'a alias = 'a array
(let (mut_arr/471 =[value<intarray>] (makearray[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/471))
val mut_arr : int alias = [|1; 2; 3|]
|}];;

let _ = mut_arr.(1);;
[%%expect {|
(let (mut_arr/471 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/471 1))
- : int = 2
|}]

let _ = Array.get mut_arr 1;;
[%%expect {|
(let (mut_arr/471 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/471 1))
- : int = 2
|}]

let _ = Array.unsafe_get mut_arr 1;;
[%%expect {|
(let (mut_arr/471 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.unsafe_get[int indexed by int] mut_arr/471 1))
- : int = 2
|}]

let arr = [: 1; 2; 3 :];;
let mut_arr = to_array arr;;
[%%expect {|
(let (arr/472 =[value<intarray>] (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/472))
val arr : int iarray = [:1; 2; 3:]
(let
  (arr/472 =? (apply (field_imm 0 (global Toploop!)) "arr")
   mut_arr/473 =[value<intarray>]
     (apply (field_imm 12 (global Stdlib_stable__IarrayLabels!)) arr/472))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/473))
val mut_arr : int array = [|1; 2; 3|]
|}];;

let _ = mut_arr.(1);;
[%%expect {|
(let (mut_arr/473 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/473 1))
- : int = 2
|}]

let _ = Array.get mut_arr 1;;
[%%expect {|
(let (mut_arr/473 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/473 1))
- : int = 2
|}]

let _ = Array.unsafe_get mut_arr 1;;
[%%expect {|
(let (mut_arr/473 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.unsafe_get[int indexed by int] mut_arr/473 1))
- : int = 2
|}]
