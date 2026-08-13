(* TEST
 flags = "-extension layouts";
 expect;
*)

(* Type declarations with a field or constructor argument of kind [any] get a
   "variable representation", which requires [-extension layouts_beta]. At the
   weaker [layouts] (stable) level, declaring such a type is an error. See also
   [any_in_record.ml], [any_in_variant.ml], and [any_in_unboxed_record.ml],
   which exercise these declarations at [layouts_alpha].

   A single-field [@@unboxed] record (or single-argument [@@unboxed] variant)
   with an [any]-kinded field is also rejected: although it gets an [unboxed]
   representation tag rather than an explicit variable one, that representation
   is implicitly variable (it carries no sort), so it requires beta too. *)

type t_any : any
[%%expect{|
type t_any : any
|}]

(* Boxed record with an [any]-kinded field. *)
type r = { x : t_any }
[%%expect{|
Line 1, characters 0-22:
1 | type r = { x : t_any }
    ^^^^^^^^^^^^^^^^^^^^^^
Error: This construct requires the beta version of the extension "layouts", which is disabled and cannot be used
|}]

type ('a : any) r' = { x : 'a }
[%%expect{|
Line 1, characters 0-31:
1 | type ('a : any) r' = { x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This construct requires the beta version of the extension "layouts", which is disabled and cannot be used
|}]

(* Variant with an [any]-kinded constructor argument. *)
type v = V of t_any
[%%expect{|
Line 1, characters 0-19:
1 | type v = V of t_any
    ^^^^^^^^^^^^^^^^^^^
Error: This construct requires the beta version of the extension "layouts", which is disabled and cannot be used
|}]

type ('a : any) v' = V of 'a
[%%expect{|
Line 1, characters 0-28:
1 | type ('a : any) v' = V of 'a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This construct requires the beta version of the extension "layouts", which is disabled and cannot be used
|}]

(* Variant with an inline record containing an [any]-kinded field. *)
type vr = VR of { x : t_any }
[%%expect{|
Line 1, characters 0-29:
1 | type vr = VR of { x : t_any }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This construct requires the beta version of the extension "layouts", which is disabled and cannot be used
|}]

type ('a : any) vr' = VR of { x : 'a }
[%%expect{|
Line 1, characters 0-38:
1 | type ('a : any) vr' = VR of { x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This construct requires the beta version of the extension "layouts", which is disabled and cannot be used
|}]

(* Unboxed-product record with an [any]-kinded field. *)
type ur = #{ x : t_any }
[%%expect{|
Line 1, characters 0-24:
1 | type ur = #{ x : t_any }
    ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This construct requires the beta version of the extension "layouts", which is disabled and cannot be used
|}]

type ('a : any) ur' = #{ x : 'a }
[%%expect{|
Line 1, characters 0-33:
1 | type ('a : any) ur' = #{ x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This construct requires the beta version of the extension "layouts", which is disabled and cannot be used
|}]

(* [@@unboxed] records and variants whose field has kind [any] have an
   implicitly variable representation, so they require beta as well. *)
type ru = { x : t_any } [@@unboxed]
[%%expect{|
Line 1, characters 0-35:
1 | type ru = { x : t_any } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This construct requires the beta version of the extension "layouts", which is disabled and cannot be used
|}]

type ('a : any) ru' = { x : 'a } [@@unboxed]
[%%expect{|
Line 1, characters 0-44:
1 | type ('a : any) ru' = { x : 'a } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This construct requires the beta version of the extension "layouts", which is disabled and cannot be used
|}]

type vu = VU of t_any [@@unboxed]
[%%expect{|
Line 1, characters 0-33:
1 | type vu = VU of t_any [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This construct requires the beta version of the extension "layouts", which is disabled and cannot be used
|}]

type ('a : any) vu' = VU of 'a [@@unboxed]
[%%expect{|
Line 1, characters 0-42:
1 | type ('a : any) vu' = VU of 'a [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This construct requires the beta version of the extension "layouts", which is disabled and cannot be used
|}]
