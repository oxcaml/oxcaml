(* TEST
 flags = "-extension laws -dsource";
 expect;
*)

(* Laws in structures and signatures, with and without parameters and
   assumptions. *)

let length = List.length

law_? length_nonneg (xs : 'a list) : length xs >= 0
[%%expect {|

let length = List.length;;
val length : 'a list -> int = <fun>

law_? length_nonneg (xs : 'a list) : (length xs) >= 0;;
law_? length_nonneg (xs : 'a list) : (length xs) >= (0)
|}]

law_? unzip_zip (xs : int list) (ys : int list) :
  length xs = length ys implies_
  let xs', ys' = List.split (List.combine xs ys) in
  xs' = xs && ys' = ys
[%%expect {|

law_? unzip_zip (xs : int list) (ys : int list) :
  (length xs) = (length ys) implies_
  (let (xs', ys') = List.split (List.combine xs ys) in
   (xs' = xs) && (ys' = ys));;
Lines 3-4, characters 2-22:
3 | ..let xs', ys' = List.split (List.combine xs ys) in
4 |   xs' = xs && ys' = ys
Error: Laws do not support let-bindings.
|}]

module M = struct
  law_? trivial : true
end
[%%expect {|

module M = struct law_? trivial : true end;;
Line 2, characters 18-22:
2 |   law_? trivial : true
                      ^^^^
Error: Laws do not support constructors.
|}]

module type S = sig
  val f : int -> int
  law_? f_id (x : int) : x >= 0 implies_ f x = x [@@attr]
end
[%%expect {|

module type S  =
  sig
    val f : int -> int
    law_? f_id (x : int) : x >= 0 implies_ (f x) = x[@@attr ]
  end;;
module type S =
  sig
    val f : int -> int
    law_? f_id (x : int) : x >= (0) implies_ (f x) = x
  end
|}]
