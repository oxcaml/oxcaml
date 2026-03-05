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
Line 9, characters 26-35:
9 |   let foo (x: 'a) = <[fun (y : $'a) -> 1]>;;
                              ^^^^^^^^^
Error: This pattern matches values of type "$('a)"
       but a pattern was expected which matches values of type
         "('b : '_representable_layout_1)"
       The layout of $('a) is any
         because it's assigned a dummy kind that should have been overwritten.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of $('a) must be representable
         because we must know concretely how to pass a function argument.
|}]

module M1' : sig
  val foo : <['a]> expr -> <['a -> int]> expr
end = struct
  let foo (x: 'a expr) = <[fun (y : $'a) -> 1]>;;
end

[%%expect{|
Line 4, characters 31-40:
4 |   let foo (x: 'a expr) = <[fun (y : $'a) -> 1]>;;
                                   ^^^^^^^^^
Error: This pattern matches values of type "$('a)"
       but a pattern was expected which matches values of type
         "('b : '_representable_layout_2)"
       The layout of $('a) is any
         because it's assigned a dummy kind that should have been overwritten.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of $('a) must be representable
         because we must know concretely how to pass a function argument.
|}]

module M1'' : sig
  val foo : 'a expr -> <[$('a) -> int]> expr
end = struct
  let foo (x: <['a]> expr) = <[fun (y : 'a) -> 1]>;;
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let foo (x: <['a]> expr) = <[fun (y : 'a) -> 1]>;;
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val foo :
             ('a : <[value_or_null]>). 'a expr -> <[$('a) -> int]> expr
         end
       is not included in
         sig val foo : 'a expr -> <[$('a) -> int]> expr end
       Values do not match:
         val foo : ('a : <[value_or_null]>). 'a expr -> <[$('a) -> int]> expr
       is not included in
         val foo : 'a expr -> <[$('a) -> int]> expr
       The type "'a expr -> <[$('a) -> int]> expr"
       is not compatible with the type "'b expr -> <[$('b) -> int]> expr"
       Type "'a" is not compatible with type "'b"
       The kind of 'a is value
         because of the definition of foo at line 2, characters 2-44.
       But the kind of 'a must be a subkind of <[value_or_null]>
         because of the definition of foo at line 4, characters 10-50.
|}]

(*  Simple functions with a type variable under a quote-splice *)
module M2 = struct
  let f (x : <[ 'a ]> expr) = <[ ($x, $x) ]>
end

[%%expect{|
module M2 :
  sig val f : ('a : <[value_or_null]>). 'a expr -> <[$('a) * $('a)]> expr end
|}]

(*  Checking [M2'] does not rely on any quote-splice inverses:
    'a will be unified with <[int]> when checking the function parameter side,
    skipping any nontrivial reasoning.  *)
module M2' : sig
  val f : <[ int ]> expr -> <[ int * int ]> expr
end = M2

[%%expect{|
module M2' : sig val f : <[int]> expr -> <[int * int]> expr end
|}]

(*  [M3] is trickier, leading to the case $'a < t *)

module M3 = struct
  let f = let (x : <[ 'a -> unit ]> expr) = <[ fun _ -> () ]> in <[ ($x, $x) ]>
end

[%%expect{|
module M3 :
  sig
    val f :
      ('a : <[value_or_null]>). <[($('a) -> unit) * ($('a) -> unit)]> expr
  end
|}]

(*   $('a) = int  <=>  'a = <[int]> *)
module M3' : sig
  val f : <[ (int -> unit) * (int -> unit) ]> expr
end = M3

[%%expect{|
module M3' : sig val f : <[(int -> unit) * (int -> unit)]> expr end
|}]

(*   [M3''] is a simple failure to check the error message when we end up with
     contradicting quoted unificands *)
(*   string = $('a) = int  <=>  <[string]> = 'a = <[int]>  <=>  string = int (error!) *)
module M3'' : sig
  val f : <[ (string -> unit) * (int -> unit) ]> expr
end = M3

[%%expect{|
Line 3, characters 6-8:
3 | end = M3
          ^^
Error: Signature mismatch:
       Modules do not match:
         sig
           val f :
             ('a : <[value_or_null]>).
               <[($('a) -> unit) * ($('a) -> unit)]> expr
         end
       is not included in
         sig val f : <[(string -> unit) * (int -> unit)]> expr end
       Values do not match:
         val f :
           ('a : <[value_or_null]>).
             <[($('a) -> unit) * ($('a) -> unit)]> expr
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
module M4 : sig val x : ('a : <[value]>). <[$('a) * $('a)]> expr end
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
         sig val x : ('a : <[value]>). <[$('a) * $('a)]> expr end
       is not included in
         sig val x : <[int * string]> expr end
       Values do not match:
         val x : ('a : <[value]>). <[$('a) * $('a)]> expr
       is not included in
         val x : <[int * string]> expr
       The type "<[int * int]> expr" is not compatible with the type
         "<[int * string]> expr"
       Type "int * int" is not compatible with type "int * string"
       Type "int" is not compatible with type "string"
|}]
