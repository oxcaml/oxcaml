(* TEST
 flags = "-extension laws -dsource";
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
law? length_nonneg (xs : 'a list) : (length xs) >= 0
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
law? unzip_zip (xs : int list) (ys : int list) :
  (length xs) = (length ys) ===>
  (let (xs', ys') = List.split (List.combine xs ys) in
   (xs' = xs) && (ys' = ys))
|}]

law? trivial : true
[%%expect {|

law? trivial : true;;
law? trivial : true
|}]

module type S = sig
  val f : int -> int
  law? f_id (x : int) : x >= 0 ===> f x = x [@@attr]
end
[%%expect {|

module type S  =
  sig val f : int -> int law? f_id (x : int) : x >= 0 ===> (f x) = x[@@attr ]
  end;;
module type S =
  sig val f : int -> int law? f_id (x : int) : x >= 0 ===> (f x) = x end
|}]

(* [law] is still an ordinary identifier. *)

let law = 1
let f ~law = law
let _ = f ~law
[%%expect {|

let law = 1;;
val law : int = 1

let f ~law = law;;
val f : law:'a -> 'a = <fun>

let _ = f ~law;;
- : int = 1
|}]
