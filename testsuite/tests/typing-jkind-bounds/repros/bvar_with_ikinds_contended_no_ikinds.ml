(* TEST
 flags = "-extension small_numbers";
 expect;
*)

type ('a : value_or_null) contended_t : value_or_null mod contended = 
    { contended : 'a @@ contended } 
[@@unboxed]

type 'a data_t : value mod contended with 'a

module M : sig
  type 'a t : value mod contended
end = struct
  type 'a t = int contended_t data_t
end
[%%expect{|
type ('a : value_or_null) contended_t = { contended : 'a @@ contended; } [@@unboxed]
type 'a data_t : value mod contended with 'a
module M : sig type 'a t : value mod contended end
|}]
