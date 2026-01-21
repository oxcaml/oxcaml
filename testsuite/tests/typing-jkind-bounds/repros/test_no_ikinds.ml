(* TEST
 flags = "-extension small_numbers";
 expect;
*)

module M : sig
  type ('a : value_or_null) contended_t : value_or_null mod contended 
end = struct
  type ('a : value_or_null) contended_t : value_or_null mod contended = 
      { contended : 'a @@ contended } 
  [@@unboxed]
end

type 'a data_t : value mod contended with 'a

module M : sig
  type 'a t : value mod contended
end = struct
  type 'a t = int M.contended_t data_t
end
[%%expect{|
module M :
  sig type ('a : value_or_null) contended_t : value_or_null mod contended end
type 'a data_t : value mod contended with 'a
Line 14, characters 14-31:
14 |   type 'a t = int M.contended_t data_t
                   ^^^^^^^^^^^^^^^^^
Error: This type "int M.contended_t" should be an instance of type "('a : value)"
       The kind of int M.contended_t is value_or_null mod contended
         because of the definition of contended_t at line 2, characters 2-69.
       But the kind of int M.contended_t must be a subkind of value
         because of the definition of data_t at line 9, characters 0-44.
|}] 
