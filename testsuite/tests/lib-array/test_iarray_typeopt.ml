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
(let (arr/396 =[value<intarray>] (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/396))
val arr : int iarray = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/396 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/396 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/396 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/396 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/396 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/396 1))
- : int = 2
|}]

type 'a alias = 'a iarray
let arr : int alias = [: 1; 2; 3 :];;
[%%expect {|
0
type 'a alias = 'a iarray
(let (arr/398 =[value<intarray>] (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/398))
val arr : int alias = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/398 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/398 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/398 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/398 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/398 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/398 1))
- : int = 2
|}]

let mut_arr = [| 1; 2; 3 |];;
let arr = of_array mut_arr;;
[%%expect {|
(let (mut_arr/399 =[value<intarray>] (makearray[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/399))
val mut_arr : int array = [|1; 2; 3|]
(let
  (mut_arr/399 =? (apply (field_imm 0 (global Toploop!)) "mut_arr")
   arr/400 =[value<intarray>]
     (apply (field_imm 13 (global Stdlib_stable__Iarray!)) mut_arr/399))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/400))
val arr : int iarray = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/400 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/400 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/400 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/400 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/400 =? (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/400 1))
- : int = 2
|}]

(* And check that arrays are still mutable loads (array.get) *)

let mut_arr = [| 1; 2; 3 |];;
[%%expect {|
(let (mut_arr/401 =[value<intarray>] (makearray[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/401))
val mut_arr : int array = [|1; 2; 3|]
|}]

let _ = mut_arr.(1);;
[%%expect {|
(let (mut_arr/401 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/401 1))
- : int = 2
|}]

let _ = Array.get mut_arr 1;;
[%%expect {|
(let (mut_arr/401 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/401 1))
- : int = 2
|}]

let _ = Array.unsafe_get mut_arr 1;;
[%%expect {|
(let (mut_arr/401 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.unsafe_get[int indexed by int] mut_arr/401 1))
- : int = 2
|}]

type 'a alias = 'a array
let mut_arr : int alias = [| 1; 2; 3 |];;
[%%expect {|
0
type 'a alias = 'a array
(let (mut_arr/453 =[value<intarray>] (makearray[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/453))
val mut_arr : int alias = [|1; 2; 3|]
|}];;

let _ = mut_arr.(1);;
[%%expect {|
(let (mut_arr/453 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/453 1))
- : int = 2
|}]

let _ = Array.get mut_arr 1;;
[%%expect {|
(let (mut_arr/453 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/453 1))
- : int = 2
|}]

let _ = Array.unsafe_get mut_arr 1;;
[%%expect {|
(let (mut_arr/453 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.unsafe_get[int indexed by int] mut_arr/453 1))
- : int = 2
|}]

let arr = [: 1; 2; 3 :];;
let mut_arr = to_array arr;;
[%%expect {|
(let (arr/454 =[value<intarray>] (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/454))
val arr : int iarray = [:1; 2; 3:]
(let
  (arr/454 =? (apply (field_imm 0 (global Toploop!)) "arr")
   mut_arr/455 =[value<intarray>]
     (apply (field_imm 12 (global Stdlib_stable__Iarray!)) arr/454))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/455))
val mut_arr : int array = [|1; 2; 3|]
|}];;

let _ = mut_arr.(1);;
[%%expect {|
(let (mut_arr/455 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/455 1))
- : int = 2
|}]

let _ = Array.get mut_arr 1;;
[%%expect {|
(let (mut_arr/455 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/455 1))
- : int = 2
|}]

let _ = Array.unsafe_get mut_arr 1;;
[%%expect {|
(let (mut_arr/455 =? (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.unsafe_get[int indexed by int] mut_arr/455 1))
- : int = 2
|}]
