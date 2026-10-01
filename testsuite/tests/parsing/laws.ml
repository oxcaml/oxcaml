(* TEST
 flags = "-dsource";
 expect;
*)

(* Laws in structures and signatures, with and without parameters and
   assumptions. *)

let length = List.length

law? length_nonneg (xs : 'a list) : length xs >= 0
[%%expect {|

let length = List.length;;
val length : 'a list -> int = <fun>

law? length_nonneg (xs : 'a list) : (length xs) >= 0;;
Line 3, characters 0-50:
3 | law? length_nonneg (xs : 'a list) : length xs >= 0
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Laws are not supported yet.
|}]

law? unzip_zip (xs : int list) (ys : int list) :
  length xs = length ys ===>
  let xs', ys' = List.split (List.combine xs ys) in
  xs' = xs && ys' = ys
[%%expect {|

law? unzip_zip (xs : int list) (ys : int list) :
  (length xs) = (length ys) ===>
  (let (xs', ys') = List.split (List.combine xs ys) in
   (xs' = xs) && (ys' = ys));;
Lines 1-4, characters 0-22:
1 | law? unzip_zip (xs : int list) (ys : int list) :
2 |   length xs = length ys ===>
3 |   let xs', ys' = List.split (List.combine xs ys) in
4 |   xs' = xs && ys' = ys
Error: Laws are not supported yet.
|}]

module M = struct
  law? trivial : true
end
[%%expect {|

module M = struct law? trivial : true end;;
Line 2, characters 2-21:
2 |   law? trivial : true
      ^^^^^^^^^^^^^^^^^^^
Error: Laws are not supported yet.
|}]

module type S = sig
  val f : int -> int
  law? f_id (x : int) : x >= 0 ===> f x = x [@@attr]
end
[%%expect {|

module type S  =
  sig val f : int -> int law? f_id (x : int) : x >= 0 ===> (f x) = x[@@attr ]
  end;;
Line 3, characters 2-52:
3 |   law? f_id (x : int) : x >= 0 ===> f x = x [@@attr]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Laws are not supported yet.
|}]
