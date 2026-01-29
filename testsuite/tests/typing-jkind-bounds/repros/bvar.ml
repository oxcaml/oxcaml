(* TEST
 flags = "-extension small_numbers";
 expect;
*)

type ('a : value_or_null) aliased_t : value_or_null mod aliased = 
    { aliased : 'a @@ aliased } 
[@@unboxed]

type 'a data_t : value mod aliased with 'a

module M : sig
  type 'a t : value mod aliased
end = struct
  type 'a t = int aliased_t data_t
end
[%%expect{|
type ('a : value_or_null) aliased_t = { aliased : 'a @@ aliased; } [@@unboxed]
type 'a data_t : value mod aliased with 'a
module M : sig type 'a t : value mod aliased end
|}]
