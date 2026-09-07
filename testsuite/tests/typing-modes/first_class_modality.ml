(* TEST
 flags = "-extension mode_alpha -w -220";
 { expect; }
 { flags += " -no-ikinds"; expect; }
*)

type t
type wrapped = (t @@ global)
type identity = (t @@ nonportable)
type nested = ((t @@ global) @@ global)
type redundant = (string @@ portable)
[%%expect{|
type t
type wrapped = (t @@ global)
type identity = (t @@ nonportable)
type nested = ((t @@ global) @@ global)
type redundant = (string @@ portable)
|}]

module type S = sig
  val value_mode : t @@ portable
  val wrapped_type : (t @@ portable)
end
type record = { value_mode : t @@ portable; wrapped_type : (t @@ portable) }
[%%expect{|
module type S =
  sig val value_mode : t @@ portable val wrapped_type : (t @@ portable) end
type record = { value_mode : t @@ portable; wrapped_type : (t @@ portable); }
|}]

let preserve (x : (t @@ global)) = x
let identity x = x
let transport (x : (t @@ global)) = identity x
let infer_payload (xs : ('a @@ global) list) =
  (xs : (string @@ global) list)
let bind_whole (xs : 'a list) = (xs : (string @@ global) list)
[%%expect{|
val preserve : (t @@ global) -> (t @@ global) = <fun>
val identity : 'a -> 'a = <fun>
val transport : (t @@ global) -> (t @@ global) = <fun>
val infer_payload : (string @@ global) list -> (string @@ global) list =
  <fun>
val bind_whole : (string @@ global) list -> (string @@ global) list = <fun>
|}]

(* None of these wrappers is erased by crossing or idempotence. *)
let no_crossing_equality (xs : (string @@ portable) list) =
  (xs : string list)
[%%expect{|
Line 2, characters 3-5:
2 |   (xs : string list)
       ^^
Error: The value "xs" has type "(string @@ portable) list"
       but an expression was expected of type "string list"
       Type "(string @@ portable)" is not compatible with type "string"
|}]

let no_identity_equality (xs : (int @@ nonportable) list) = (xs : int list)
[%%expect{|
Line 1, characters 61-63:
1 | let no_identity_equality (xs : (int @@ nonportable) list) = (xs : int list)
                                                                 ^^
Error: The value "xs" has type "(int @@ nonportable) list"
       but an expression was expected of type "int list"
       Type "(int @@ nonportable)" is not compatible with type "int"
|}]

let no_idempotence (xs : ((t @@ global) @@ global) list) =
  (xs : (t @@ global) list)
[%%expect{|
Line 2, characters 3-5:
2 |   (xs : (t @@ global) list)
       ^^
Error: The value "xs" has type "((t @@ global) @@ global) list"
       but an expression was expected of type "(t @@ global) list"
       Type "(t @@ global)" is not compatible with type "t"
|}]

let no_composition (xs : ((t @@ portable) @@ contended) list) =
  (xs : (t @@ portable contended) list)
[%%expect{|
Line 2, characters 3-5:
2 |   (xs : (t @@ portable contended) list)
       ^^
Error: The value "xs" has type "((t @@ portable) @@ contended) list"
       but an expression was expected of type "(t @@ portable contended) list"
       Type "((t @@ portable) @@ contended)" is not compatible with type
         "(t @@ portable contended)"
|}]

let different_labels (xs : (t @@ global) list) =
  (xs : (t @@ global portable) list)
[%%expect{|
Line 2, characters 3-5:
2 |   (xs : (t @@ global portable) list)
       ^^
Error: The value "xs" has type "(t @@ global) list"
       but an expression was expected of type "(t @@ global portable) list"
       Type "(t @@ global)" is not compatible with type "(t @@ global portable)"
|}]

let self_equation (xs : 'a list) = (xs : ('a @@ global) list)
[%%expect{|
Line 1, characters 36-38:
1 | let self_equation (xs : 'a list) = (xs : ('a @@ global) list)
                                        ^^
Error: The value "xs" has type "'a list" but an expression was expected of type
         "('a @@ global) list"
       The type variable "'a" occurs inside "('a @@ global)"
|}]

type recursive_alias = (recursive_alias @@ global)
[%%expect{|
Line 1, characters 0-50:
1 | type recursive_alias = (recursive_alias @@ global)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type abbreviation "recursive_alias" is cyclic:
         "recursive_alias" = "(recursive_alias @@ global)",
         "(recursive_alias @@ global)" contains "recursive_alias"
|}]

type guarded = Node of (guarded @@ global)
type unboxed = (float# @@ portable)
type product = (#(float# * int) @@ global)
[%%expect{|
type guarded = Node of (guarded @@ global)
type unboxed = (float# @@ portable)
type product = (#(float# * int) @@ global)
|}]

(* Kind computation still applies the modality, without type equality. *)
type global : value mod global = (t @@ global)
type portable : value mod portable = (t @@ portable)
type contended : value mod contended = (t @@ contended)
type many : value mod many = (t @@ many)
type aliased : value mod aliased = (t @@ aliased)
type forkable : value mod forkable = (t @@ forkable)
type unyielding : value mod unyielding = (t @@ unyielding)
type stateless : value mod stateless = (t @@ stateless)
type immutable : value mod immutable = (t @@ immutable)
type dynamic : value mod dynamic = (t @@ dynamic)
[%%expect{|
type global = (t @@ global)
type portable = (t @@ portable)
type contended = (t @@ contended)
type many = (t @@ many)
type aliased = (t @@ aliased)
type forkable = (t @@ forkable)
type unyielding = (t @@ unyielding)
type stateless = (t @@ stateless)
type immutable = (t @@ immutable)
type dynamic = (t @@ dynamic)
|}]

type ('a : value mod portable) needs_portable
type 'a residual = ('a @@ shareable) needs_portable
[%%expect{|
type ('a : value mod portable) needs_portable
type ('a : value mod corruptible) residual = ('a @@ shareable) needs_portable
|}]

type (!'a) injective = ('a @@ global)
type (+'a) covariant = ('a @@ portable)
[%%expect{|
type 'a injective = ('a @@ global)
type 'a covariant = ('a @@ portable)
|}]

module Matching : sig
  val id : ('a @@ global) -> ('a @@ global)
end = struct
  let id x = x
end
[%%expect{|
module Matching : sig val id : ('a @@ global) -> ('a @@ global) end @@
  stateless
|}]

module Different : sig type t = (string @@ global portable) end = struct
  type t = (string @@ global)
end
[%%expect{|
Lines 1-3, characters 66-3:
1 | ..................................................................struct
2 |   type t = (string @@ global)
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = (string @@ global) end
       is not included in
         sig type t = (string @@ global portable) end
       Type declarations do not match:
         type t = (string @@ global)
       is not included in
         type t = (string @@ global portable)
       The type "(string @@ global)" is not equal to the type
         "(string @@ global portable)"
|}]

module Nested : sig type t = ((string @@ global) @@ global) end = struct
  type t = (string @@ global)
end
[%%expect{|
Lines 1-3, characters 66-3:
1 | ..................................................................struct
2 |   type t = (string @@ global)
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = (string @@ global) end
       is not included in
         sig type t = ((string @@ global) @@ global) end
       Type declarations do not match:
         type t = (string @@ global)
       is not included in
         type t = ((string @@ global) @@ global)
       The type "(string @@ global)" is not equal to the type
         "((string @@ global) @@ global)"
       Type "string" is not equal to type "(string @@ global)"
|}]

type (_, _) equality = Refl : ('a, 'a) equality
let inject : type a b.
  ((a @@ global), (b @@ global)) equality -> a -> b =
  fun Refl x -> x
[%%expect{|
type (_, _) equality = Refl : ('a, 'a) equality
val inject : (('a @@ global), ('b @@ global)) equality -> 'a -> 'b = <fun>
|}]

let disjoint : type a b.
  ((a @@ global), (b @@ portable)) equality -> unit = function
  | _ -> .
[%%expect{|
val disjoint : (('a @@ global), ('b @@ portable)) equality -> unit = <fun>
|}]

let recursive_witness (type a)
    (witness : (a, (a @@ global)) equality) =
  match witness with Refl -> ()
[%%expect{|
val recursive_witness : ('a, ('a @@ global)) equality -> unit = <fun>
|}]

let no_recursive_cast (type a)
    (witness : (a, (a @@ global)) equality) (xs : a list)
    : (a @@ global) list =
  match witness with Refl -> xs
[%%expect{|
Line 4, characters 29-31:
4 |   match witness with Refl -> xs
                                 ^^
Error: The value "xs" has type "a list" but an expression was expected of type
         "(a @@ global) list"
       Type "a" is not compatible with type "(a @@ global)"
|}]

type _ reveals = Reveal : (string @@ global) reveals
let revealed (type a) (witness : a reveals) (x : a) =
  match witness with Reveal -> (x : string)
[%%expect{|
type _ reveals = Reveal : (string @@ global) reveals
val revealed : 'a reveals -> 'a -> string = <fun>
|}]
