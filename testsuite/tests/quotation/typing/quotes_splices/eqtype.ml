(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

(** Type equality with [eqtype] **)

module M : sig
  type 'a t = <[$('a) -> $('a)]> expr
end = struct
  type 'a t = <[$('a) -> $('a)]> expr
end
[%%expect{|
module M : sig type 'a t = <[$('a) -> $('a)]> expr end
|}]

module M : sig
  type t = <[int]> expr
end = struct
  type t = <[int]> expr
end
[%%expect{|
module M : sig type t = <[int]> expr end
|}]

module M : sig
  type t = <[int]> expr
end = struct
  type t = <[string]> expr
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <[string]> expr
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = <[string]> expr end
       is not included in
         sig type t = <[int]> expr end
       Type declarations do not match:
         type t = <[string]> expr
       is not included in
         type t = <[int]> expr
       The type "<[string]> expr" is not equal to the type "<[int]> expr"
       Type "string" is not equal to type "int"
|}]

module M : sig
  type t = <[<[int]> expr -> <[int]> expr]> expr
end = struct
  type t = <[<[int]> expr -> <[int]> expr]> expr
end
[%%expect{|
module M : sig type t = <[<[int]> expr -> <[int]> expr]> expr end
|}]
