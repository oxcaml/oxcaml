(* TEST
 flags = "-extension-universe alpha";
 expect;
*)

(* Tests for [immediate_or_null]. *)

(* The [immediate_or_null] layout. *)
type t_immediate_or_null : immediate_or_null
[%%expect{|
type t_immediate_or_null : immediate_or_null
|}]

(* The subkind relation. *)

type ('a : immediate_or_null) accept_immediate_or_null
[%%expect{|
type ('a : immediate_or_null) accept_immediate_or_null
|}]

type should_work = t_immediate_or_null accept_immediate_or_null
[%%expect{|
type should_work = t_immediate_or_null accept_immediate_or_null
|}]

type should_work = int accept_immediate_or_null
[%%expect{|
type should_work = int accept_immediate_or_null
|}]

type ('a : immediate) accept_immediate
[%%expect{|
type ('a : immediate) accept_immediate
|}]

type should_fail = t_immediate_or_null accept_immediate
[%%expect{|
Line 1, characters 19-38:
1 | type should_fail = t_immediate_or_null accept_immediate
                       ^^^^^^^^^^^^^^^^^^^
Error: This type "t_immediate_or_null" should be an instance of type
         "('a : immediate)"
       The layout of t_immediate_or_null is value non_pointer maybe_null
         because of the definition of t_immediate_or_null at line 1, characters 0-44.
       But the layout of t_immediate_or_null must be a sublayout of immediate
         because of the definition of accept_immediate at line 1, characters 0-38.
|}]

type ('a : value) accept_value
[%%expect{|
type 'a accept_value
|}]

type should_fail = t_immediate_or_null accept_value
[%%expect{|
Line 1, characters 19-38:
1 | type should_fail = t_immediate_or_null accept_value
                       ^^^^^^^^^^^^^^^^^^^
Error: This type "t_immediate_or_null" should be an instance of type
         "('a : value)"
       The layout of t_immediate_or_null is value non_pointer maybe_null
         because of the definition of t_immediate_or_null at line 1, characters 0-44.
       But the layout of t_immediate_or_null must be a sublayout of value
         because of the definition of accept_value at line 1, characters 0-30.
|}]

type ('a : value_or_null) accept_value_or_null

type should_work = t_immediate_or_null accept_value_or_null
[%%expect{|
type ('a : value_or_null) accept_value_or_null
type should_work = t_immediate_or_null accept_value_or_null
|}]

(* [int or_null] fits into [immediate_or_null]: *)


type int_or_null : immediate_or_null = int or_null
[%%expect{|
type int_or_null = int or_null
|}]

type should_work = int_or_null accept_immediate_or_null
[%%expect{|
type should_work = int_or_null accept_immediate_or_null
|}]

type should_work = int or_null accept_immediate_or_null
[%%expect{|
type should_work = int or_null accept_immediate_or_null
|}]

(* Values. *)

type ('a : immediate_or_null) myref = { mutable v : 'a }

external read_imm : ('a : immediate_or_null) . 'a myref -> 'a = "%field0"
external write_imm : ('a : immediate_or_null) . 'a myref -> 'a -> unit = "%setfield0"
external equal : ('a : immediate_or_null) . 'a -> 'a -> bool = "%equal"

[%%expect{|
type ('a : immediate_or_null) myref = { mutable v : 'a; }
external read_imm : ('a : immediate_or_null). 'a myref -> 'a = "%field0"
external write_imm : ('a : immediate_or_null). 'a myref -> 'a -> unit
  = "%setfield0"
external equal : ('a : immediate_or_null). 'a -> 'a -> bool = "%equal"
|}]

let () =
  let r = { v = (Null : int or_null) } in
  let x = read_imm r in
  assert (equal x Null);
  write_imm r (This 5);
  assert (equal r.v (This 5))
;;

[%%expect{|
|}]

type ('a : value_or_null mod non_float) accepts_nonfloat

type succeeds = t_immediate_or_null accepts_nonfloat

[%%expect{|
type ('a : value_or_null non_float) accepts_nonfloat
type succeeds = t_immediate_or_null accepts_nonfloat
|}]

(* Values of [int or_null] mode-cross: *)

let f (x : int or_null @ local) (g : int or_null -> unit) = g x [@nontail]

[%%expect{|
val f : int or_null @ local -> ((int or_null -> unit) -> unit) = <fun>
|}, Principal{|
val f : int or_null @ local -> (int or_null -> unit) -> unit = <fun>
|}]

module M : sig
  type t : immediate_or_null
end = struct
  type t = int or_null
end

[%%expect{|
module M : sig type t : immediate_or_null end @@ stateless
|}]

(* Tests for [immediate64_or_null]. *)

type t_immediate64_or_null : immediate64_or_null
[%%expect{|
type t_immediate64_or_null : immediate64_or_null
|}]

type ('a : immediate64_or_null) accept_immediate64_or_null
[%%expect{|
type ('a : immediate64_or_null) accept_immediate64_or_null
|}]

type should_work = t_immediate64_or_null accept_immediate64_or_null
[%%expect{|
type should_work = t_immediate64_or_null accept_immediate64_or_null
|}]

type ('a : immediate64) accept_immediate64
[%%expect{|
type ('a : immediate64) accept_immediate64
|}]

type should_fail = t_immediate64_or_null accept_immediate64
[%%expect{|
Line 1, characters 19-40:
1 | type should_fail = t_immediate64_or_null accept_immediate64
                       ^^^^^^^^^^^^^^^^^^^^^
Error: This type "t_immediate64_or_null" should be an instance of type
         "('a : immediate64)"
       The layout of t_immediate64_or_null is value non_pointer maybe_null
         because of the definition of t_immediate64_or_null at line 1, characters 0-48.
       But the layout of t_immediate64_or_null must be a sublayout of
           immediate64
         because of the definition of accept_immediate64 at line 1, characters 0-42.
|}]

type should_fail = t_immediate64_or_null accept_immediate
[%%expect{|
Line 1, characters 19-40:
1 | type should_fail = t_immediate64_or_null accept_immediate
                       ^^^^^^^^^^^^^^^^^^^^^
Error: This type "t_immediate64_or_null" should be an instance of type
         "('a : immediate)"
       The layout of t_immediate64_or_null is value non_pointer maybe_null
         because of the definition of t_immediate64_or_null at line 1, characters 0-48.
       But the layout of t_immediate64_or_null must be a sublayout of
           immediate
         because of the definition of accept_immediate at line 1, characters 0-38.
|}]

type should_fail = t_immediate64_or_null accept_value
[%%expect{|
Line 1, characters 19-40:
1 | type should_fail = t_immediate64_or_null accept_value
                       ^^^^^^^^^^^^^^^^^^^^^
Error: This type "t_immediate64_or_null" should be an instance of type
         "('a : value)"
       The layout of t_immediate64_or_null is value non_pointer maybe_null
         because of the definition of t_immediate64_or_null at line 1, characters 0-48.
       But the layout of t_immediate64_or_null must be a sublayout of value
         because of the definition of accept_value at line 1, characters 0-30.
|}]

type should_work = t_immediate64_or_null accept_value_or_null
[%%expect{|
type should_work = t_immediate64_or_null accept_value_or_null
|}]

type should_work = t_immediate_or_null accept_immediate64_or_null
[%%expect{|
type should_work = t_immediate_or_null accept_immediate64_or_null
|}]

type succeeds64 = t_immediate64_or_null accepts_nonfloat
[%%expect{|
type succeeds64 = t_immediate64_or_null accepts_nonfloat
|}]

(* CR with-kinds: fix the difference with principality. *)
type should_fail = exn or_null accept_immediate64_or_null
[%%expect{|
Line 1, characters 19-30:
1 | type should_fail = exn or_null accept_immediate64_or_null
                       ^^^^^^^^^^^
Error: This type "exn or_null" should be an instance of type
         "('a : immediate64_or_null)"
       The layout of exn or_null is value maybe_separable maybe_null
         because it is the primitive type or_null.
       But the layout of exn or_null must be a sublayout of
           value non_pointer maybe_null
         because of the definition of accept_immediate64_or_null at line 1, characters 0-58.
|}]

module M64 : sig
  type t : immediate64_or_null
end = struct
  type t = int or_null
end

[%%expect{|
module M64 : sig type t : immediate64_or_null end @@ stateless
|}]

module Fails : sig
  type t : immediate64_or_null
end = struct
  type t = string or_null
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = string or_null
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = string or_null end
       is not included in
         sig type t : immediate64_or_null end
       Type declarations do not match:
         type t = string or_null
       is not included in
         type t : immediate64_or_null
       The layout of the first is value maybe_separable maybe_null
         because it is the primitive type or_null.
       But the layout of the first must be a sublayout of
           value non_pointer maybe_null
         because of the definition of t at line 2, characters 2-30.
|}]
