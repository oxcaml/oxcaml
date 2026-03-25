(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

(** Inclusion checks with [moregen] **)

(*  [M1] and [M2] pass solely due to stage normalisation
    (moving all type variables to occur at stage offset zero) *)

module M1 : sig
  val foo : 'a -> <[$('a) -> int]> expr
end = struct
  let foo (x: 'a) = <[fun (y : $'a) -> 1]>;;
end

[%%expect{|
module M1 : sig val foo : 'a -> <[$('a) -> int]> expr end
|}]

module M1' : sig
  val foo : <['a]> expr -> <['a -> int]> expr
end = struct
  let foo (x: 'a expr) = <[fun (y : $'a) -> 1]>;;
end

[%%expect{|
module M1' : sig val foo : 'a expr -> <[$('a) -> int]> expr end
|}]

module M1'' : sig
  val foo : 'a expr -> <[$('a) -> int]> expr
end = struct
  let foo (x: <['a]> expr) = <[fun (y : 'a) -> 1]>;;
end

[%%expect{|
module M1'' : sig val foo : 'a expr -> <[$('a) -> int]> expr end
|}]

(*  Simple functions with a type variable under a quote-splice *)
module M2 = struct
  let f (x : <[ 'a ]> expr) = <[ ($x, $x) ]>
end

[%%expect{|
module M2 : sig val f : 'a expr -> <[$('a) * $('a)]> expr end
|}]

(*  Checking [M2'] does not rely on any quote-splice inverses:
    'a will be unified with <[int]> when checking the function parameter side,
    skipping any nontrivial reasoning.  *)
module M2' : sig
  val f : <[ int ]> expr -> <[ int * int ]> expr @ once
end = M2

[%%expect{|
module M2' : sig val f : <[int]> expr -> <[int * int]> expr end
|}]

(*  [M3] is trickier, leading to the case $'a < t *)

module M3 = struct
  let f () =
    let (x : <[ 'a -> unit ]> expr) = <[ fun _ -> () ]> in
    <[ ($x, $x) ]>
end

[%%expect{|
module M3 : sig val f : <[($('a) -> unit) * ($('a) -> unit)]> expr end
|}]

(*   $('a) = int  <=>  'a = <[int]> *)
module M3' : sig
  val f : unit -> <[ (int -> unit) * (int -> unit) ]> expr @ once
end = M3

[%%expect{|
module M3' : sig val f : <[(int -> unit) * (int -> unit)]> expr end
|}]

(*   [M3''] is a simple failure to check the error message when we end up with
     contradicting quoted unificands *)
(*   string = $('a) = int  <=>  <[string]> = 'a = <[int]>  <=>  string = int (error!) *)
module M3'' : sig
  val f : unit -> <[ (string -> unit) * (int -> unit) ]> expr @ once
end = M3

[%%expect{|
Line 3, characters 6-8:
3 | end = M3
          ^^
Error: Signature mismatch:
       Modules do not match:
         sig val f : <[($('a) -> unit) * ($('a) -> unit)]> expr end
       is not included in
         sig val f : <[(string -> unit) * (int -> unit)]> expr end
       Values do not match:
         val f : <[($('a) -> unit) * ($('a) -> unit)]> expr
       is not included in
         val f : <[(string -> unit) * (int -> unit)]> expr
       The type "<[(string -> unit) * (string -> unit)]> expr"
       is not compatible with the type
         "<[(string -> unit) * (int -> unit)]> expr"
       Type "(string -> unit) * (string -> unit)" is not compatible with type
         "(string -> unit) * (int -> unit)"
       Type "string" is not compatible with type "int"
|}]

(*  [M4] is analogous to [M3], but featuring an uninhabited type *)

module M4 : sig
  val x : <[ 'a * 'a ]> expr
end = struct
  let x = <[ let y = Obj.magic () in (y, y) ]>
end

[%%expect{|
module M4 : sig val x : <[$('a) * $('a)]> expr end
|}]

module M4' : sig
  val x : <[ int * int ]> expr
end = M4

[%%expect{|
module M4' : sig val x : <[int * int]> expr end
|}]

(* Analogously to [M3''], [M4''] checks we detect any failing unifications *)
module M4'' : sig
  val x : <[ int * string ]> expr
end = M4

[%%expect{|
Line 3, characters 6-8:
3 | end = M4
          ^^
Error: Signature mismatch:
       Modules do not match:
         sig val x : <[$('a) * $('a)]> expr end
       is not included in
         sig val x : <[int * string]> expr end
       Values do not match:
         val x : <[$('a) * $('a)]> expr
       is not included in
         val x : <[int * string]> expr
       The type "<[int * int]> expr" is not compatible with the type
         "<[int * string]> expr"
       Type "int * int" is not compatible with type "int * string"
       Type "int" is not compatible with type "string"
|}]
