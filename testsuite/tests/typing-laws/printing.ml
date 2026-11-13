(* TEST
 flags = "-extension laws";
 expect;
*)

(* [-i] prints laws with the types of their parameters and their clauses. *)

module type Basic = sig
  type t
  val create : int -> t
  val to_int : t -> int
  law_? roundtrip (x : t) (f : int -> t) :
    to_int (create 1) = 1 implies_ to_int (f 1) >= 0 implies_ to_int x = to_int x
end
[%%expect {|
module type Basic =
  sig
    type t
    val create : int -> t
    val to_int : t -> int
    law_? roundtrip (x : t) (f : int -> t) :
      (to_int (create (1))) = (1) implies_ (to_int (f (1))) >= (0) implies_
      (to_int x) = (to_int x)
  end
|}]

(* The kind of a type variable shared by the parameters is printed at its
   first occurrence if it is not value. *)

module type Kinded = sig
  val f : ('a : immutable_data) -> bool
  law_? kinded (x : 'a) (y : ('a : immutable_data)) : f x && f y
  law_? kinded_unused (x : ('a : immutable_data)) : 1 = 1
  law_? plain (xs : 'a list) : 1 = 1
end
[%%expect {|
module type Kinded =
  sig
    val f : ('a : immutable_data). 'a -> bool
    law_? kinded (x : ('a : immutable_data)) (y : 'a) : (f x) && (f y)
    law_? kinded_unused (x : ('a : immutable_data)) : (1) = (1)
    law_? plain (xs : 'a list) : (1) = (1)
  end
|}]

(* Names that are keywords are printed escaped, for laws and parameters. *)

module type Raw_names = sig
  val \#method : int -> int
  law_? \#type (\#method : int) : \#method = \#method
  law_? \#end : \#method 1 = \#method 1
end
[%%expect {|
module type Raw_names =
  sig
    val \#method : int -> int
    law_? \#type (\#method : int) : \#method = \#method
    law_? \#end : (\#method (1)) = (\#method (1))
  end
|}]

(* Type annotations are mandatory in law parameters. The example below
   illustrates why. Without type annotations, we would not have a typedtree
   type expression to use for printing. Instead, we would have to go via the
   [out_type.ml] printing. The example below shows a case where the outtype
   printing (used for [-i]) does not yield the correct type: the annotation
   [int] of [x] is linked to the [t] of [f], which [N.t] shadows, so [x]
   is printed with the type [t/2], which does not exist at the source level. *)

type t = int
let f (x : t) : t = x
module N = struct
  type t = string
  law_? l (x : int) : f x = x
end
[%%expect {|
type t = int
val f : t -> t = <fun>
module N : sig type t = string law_? l (x : t/2) : (f x) = x end
|}]
