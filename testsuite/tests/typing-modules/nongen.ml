(* TEST
 expect;
*)

module X = struct
  let t = ref None
end

module type X' = module type of X
[%%expect{|
module X : sig val t : '_weak1 option ref end
Line 5, characters 32-33:
5 | module type X' = module type of X
                                    ^
Error: The type of this module, sig val t : '_weak1 option ref end,
       contains non-generalizable type variable(s).
       (see manual section 6.1.2)
Line 2, characters 6-7:
2 |   let t = ref None
          ^
  The type of this value, "'_weak1 option ref",
  contains the non-generalizable type variable(s) "'_weak1".
|}]

(* The check is vacuous, and skipped, for a path rooted at a persistent unit. *)

module type Unit' = module type of Stdlib.Unit
[%%expect{|
module type Unit' =
  sig
    type t = unit = ()
    val equal : t -> t -> bool @@ portable
    val compare : t -> t -> int @@ portable
    val to_string : t -> string @@ portable
  end
|}]

(* A local path still runs the check, whether reached through an alias... *)

module Y = X
module type Y' = module type of Y
[%%expect{|
module Y = X
Line 2, characters 32-33:
2 | module type Y' = module type of Y
                                    ^
Error: The type of this module, sig val t : '_weak1 option ref end,
       contains non-generalizable type variable(s).
       (see manual section 6.1.2)
Line 2, characters 6-7:
2 |   let t = ref None
          ^
  The type of this value, "'_weak1 option ref",
  contains the non-generalizable type variable(s) "'_weak1".
|}]

(* ... or as the root of a dotted path. *)

module M = struct module N = struct let t = ref None end end
module type N' = module type of M.N
[%%expect{|
module M : sig module N : sig val t : '_weak2 option ref end end
Line 2, characters 32-35:
2 | module type N' = module type of M.N
                                    ^^^
Error: The type of this module, sig val t : '_weak2 option ref end,
       contains non-generalizable type variable(s).
       (see manual section 6.1.2)
Line 1, characters 40-41:
1 | module M = struct module N = struct let t = ref None end end
                                            ^
  The type of this value, "'_weak2 option ref",
  contains the non-generalizable type variable(s) "'_weak2".
|}]

(* Module expressions that are not paths are unaffected: functor application... *)

module F (Z : sig end) = struct let t = ref None end
module type F' = module type of F (struct end)
[%%expect{|
module F : functor (Z : sig end) -> sig val t : '_weak3 option ref end
Line 2, characters 32-46:
2 | module type F' = module type of F (struct end)
                                    ^^^^^^^^^^^^^^
Error: The type of this module, sig val t : '_weak3 option ref end,
       contains non-generalizable type variable(s).
       (see manual section 6.1.2)
Line 1, characters 36-37:
1 | module F (Z : sig end) = struct let t = ref None end
                                        ^
  The type of this value, "'_weak3 option ref",
  contains the non-generalizable type variable(s) "'_weak3".
|}]

(* ... and an anonymous structure. *)

module type X'' = module type of struct include X end
[%%expect{|
Line 1, characters 33-53:
1 | module type X'' = module type of struct include X end
                                     ^^^^^^^^^^^^^^^^^^^^
Error: The type of this module, sig val t : '_weak1 option ref end,
       contains non-generalizable type variable(s).
       (see manual section 6.1.2)
Line 2, characters 6-7:
2 |   let t = ref None
          ^
  The type of this value, "'_weak1 option ref",
  contains the non-generalizable type variable(s) "'_weak1".
|}]
