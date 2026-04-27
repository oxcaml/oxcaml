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
module rec Bvar :
  sig
    type 'a repr
    type ('a, 'permission) t
    external to_repr : ('a, 'b) t -> 'a repr = "%identity"
  end
|}]
