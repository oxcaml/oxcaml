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
module M2 : sig val f : 'a expr -> <[$('a) * $('a)]> expr @ once end
|}]

(*  Checking [M2'] does not rely on any quote-splice inverses:
    'a will be unified with <[int]> when checking the function parameter side,
    skipping any nontrivial reasoning.  *)
module M2' : sig
  val f : <[ int ]> expr -> <[ int * int ]> expr @ once
end = M2

[%%expect{|
module M2' : sig val f : <[int]> expr -> <[int * int]> expr @ once end
|}]

(*  [M3] is trickier, leading to the case $'a < t *)

module M3 = struct
  let f () =
    let (x : <[ 'a -> unit ]> expr) = <[ fun _ -> () ]> in
    <[ ($x, $x) ]>
end

[%%expect{|
module M3 :
  sig val f : unit -> <[($('a) -> unit) * ($('a) -> unit)]> expr @ once end
|}]

(*   $('a) = int  <=>  'a = <[int]> *)
module M3' : sig
  val f : unit -> <[ (int -> unit) * (int -> unit) ]> expr @ once
end = M3

[%%expect{|
module M3' :
  sig val f : unit -> <[(int -> unit) * (int -> unit)]> expr @ once end
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
         sig
           val f : unit -> <[($('a) -> unit) * ($('a) -> unit)]> expr @ once
         end
       is not included in
         sig
           val f : unit -> <[(string -> unit) * (int -> unit)]> expr @ once
         end
       Values do not match:
         val f : unit -> <[($('a) -> unit) * ($('a) -> unit)]> expr @ once
       is not included in
         val f : unit -> <[(string -> unit) * (int -> unit)]> expr @ once
       The type "unit -> <[(string -> unit) * (string -> unit)]> expr @ once"
       is not compatible with the type
         "unit -> <[(string -> unit) * (int -> unit)]> expr @ once"
       Type "(string -> unit) * (string -> unit)" is not compatible with type
         "(string -> unit) * (int -> unit)"
       Type "string" is not compatible with type "int"
|}]

(*  [M4] is analogous to [M3], but featuring an uninhabited type *)

module M4 : sig
  val x : <[ 'a * 'a ]> expr
end = struct
  let x = Obj.magic_many <[ let y = Obj.magic () in (y, y) ]>
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

(* Some obvious cases with misplaced quotes/splices *)

(* error -- quote missing on expr's argument in signature *)
module M : sig
  val e : ('a -> unit) expr
end = struct
  let e = <[ fun x -> () ]>
end
[%%expect{|
>> Fatal error: Ctype.decr_stage: Stage decreased below the meta stage
Uncaught exception: Typemod.Error(_, _, _)

|}]

(* error -- quote missing on expr's argument in structure (analogous) *)
module M : sig
  val e : <[$'a -> unit]> expr
end = struct
  let e : (_ -> _) expr = Obj.magic <[ fun x -> () ]>
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let e : (_ -> _) expr = Obj.magic <[ fun x -> () ]>
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val e : ('a : any) ('b : any). ('a -> 'b) expr end
       is not included in
         sig val e : <[$('a) -> unit]> expr end
       Values do not match:
         val e : ('a : any) ('b : any). ('a -> 'b) expr
       is not included in
         val e : <[$('a) -> unit]> expr
       The type "('a -> 'b) expr" is not compatible with the type
         "<[$('c) -> unit]> expr"
       Type "'a -> 'b" is not compatible with type "<[$('c) -> unit]>"
|}]

(* the correct version of the above *)
module M : sig
  val e : <[$'a -> unit]> expr
end = struct
  let e = <[ fun x -> () ]>
end
[%%expect{|
module M : sig val e : <[$('a) -> unit]> expr end
|}]
