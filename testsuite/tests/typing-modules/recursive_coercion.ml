(* TEST
 expect;
*)

(* The inclusion check for [to_repr] below must happen in an environment
   extended with the content of the module before its definition, so that we can
   see the two types are equal. It's therefore important we _don't_ do that part
   of the inclusion check when doing the simplified consistency check for
   recursive modules that happens first, because it doesn't happen in an
   extended environment. This test confirms that consistency check doesn't check
   too much.
*)
module rec Bvar : sig
  type 'a repr
  type ('a, 'permission) t

  external to_repr : ('a, _) t -> 'a repr = "%identity"
end = struct
  type 'a repr
  type ('a, 'permission) t = 'a repr

  external to_repr : 'a repr -> 'a repr = "%identity"
end

[%%expect{|
Line 1:
Error: Modules do not match:
         sig
           type 'a repr
           type ('a, 'permission) t = 'a repr
           external to_repr : 'a repr -> 'a repr = "%identity"
         end
       is not included in
         sig
           type 'a repr
           type ('a, 'permission) t
           external to_repr : ('a, 'b) t -> 'a repr = "%identity"
         end
       Values do not match:
         external to_repr : 'a repr/1 -> 'a repr/1 = "%identity"
       is not included in
         external to_repr : ('a, 'b) t/2 -> 'a repr/2 = "%identity"
       The type "'a repr/1 -> 'a repr/1" is not compatible with the type
         "('b, 'c) t/2 -> 'b repr/2"
       Type "'a repr/1" is not compatible with type "('b, 'c) t/2"
       Line 7, characters 2-14:
         Definition of type "repr/1"
       Line 8, characters 2-36:
         Definition of type "t/1"
|}]
