(* TEST
 flags = "-extension laws";
 expect;
*)

(* [-i] prints laws with the types of their parameters and their clauses. *)

module type Basic = sig
  type t
  val create : int -> t
  val to_int : t -> int
  law? roundtrip (x : t) (f : int -> t) :
    to_int (create 1) = 1 ===> to_int (f 1) >= 0 ===> to_int x = to_int x
end
[%%expect {|
module type Basic =
  sig
    type t
    val create : int -> t
    val to_int : t -> int
    law? roundtrip (x : t) (f : int -> t) : (to_int (create (1))) = (1) ===>
      (to_int (f (1))) >= (0) ===> (to_int x) = (to_int x)
  end
|}]

(* The kind of a type variable shared by the parameters is printed at its
   first occurrence. *)

module type Kinded = sig
  val f : ('a : immutable_data) -> bool
  law? kinded (x : ('a : immutable_data)) (y : 'a) : f x && f y
  law? kinded_unused (x : ('a : immutable_data)) : 1 = 1
  law? plain (xs : 'a list) : 1 = 1
end
[%%expect {|
module type Kinded =
  sig
    val f : ('a : immutable_data). 'a -> bool
    law? kinded (x : ('a : immutable_data)) (y : 'a) : (f x) && (f y)
    law? kinded_unused (x : ('a : immutable_data)) : (1) = (1)
    law? plain (xs : 'a list) : (1) = (1)
  end
|}]

(* Names that are keywords are printed escaped, for laws and parameters. *)

module type Raw_names = sig
  val \#method : int -> int
  law? \#type (\#method : int) : \#method = \#method
  law? \#end : \#method 1 = \#method 1
end
[%%expect {|
module type Raw_names =
  sig
    val \#method : int -> int
    law? \#type (\#method : int) : \#method = \#method
    law? \#end : (\#method (1)) = (\#method (1))
  end
|}]
