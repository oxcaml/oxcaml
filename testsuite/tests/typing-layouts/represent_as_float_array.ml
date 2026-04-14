(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

(* Bad [@@represent_as_float_array] attribute *)

type t = { f : float }
[@@represent_as_float_array]
[%%expect{|
Lines 1-2, characters 0-28:
1 | type t = { f : float }
2 | [@@represent_as_float_array]
Error: "[@@represent_as_float_array]" can only be used on records whose fields are all float64.
|}]

type ('a : value) t = { a : 'a }
[@@represent_as_float_array]
[%%expect{|
Lines 1-2, characters 0-28:
1 | type ('a : value) t = { a : 'a }
2 | [@@represent_as_float_array]
Error: "[@@represent_as_float_array]" can only be used on records whose fields are all float64.
|}]

type t [@@represent_as_float_array]
[%%expect{|
type t
|}]

(* Representation checks *)

module M : sig
  type r = { f : float# } [@@represent_as_float_array]
end = struct
  type r = { f : float# }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type r = { f : float# }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type r = { f : float#; } end
       is not included in
         sig type r = { f : float#; } end
       Type declarations do not match:
         type r = { f : float#; }
       is not included in
         type r = { f : float#; }
       Their internal representations differ:
       the second declaration uses float# representation.
|}]

module M : sig
  type r = { f : float# }
end = struct
  type r = { f : float# } [@@represent_as_float_array]
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type r = { f : float# } [@@represent_as_float_array]
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type r = { f : float#; } end
       is not included in
         sig type r = { f : float#; } end
       Type declarations do not match:
         type r = { f : float#; }
       is not included in
         type r = { f : float#; }
       Their internal representations differ:
       the first declaration uses float# representation.
|}]
