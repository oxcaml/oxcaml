(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_alpha";
 {
   expect;
 }
*)

(* NOTE: When adding tests to this file, consider updating
   [typing-layouts-products/separability_implicit_unboxed_records.ml] *)

type 'a r = #{ a : 'a }
and 'a ok = F : 'a r -> 'a ok [@@unboxed]
[%%expect{|
type 'a r = #{ a : 'a; }
and 'a ok = F : 'a r -> 'a ok [@@unboxed]
|}]

type 'a r = #{ a : 'a }
and 'a ok = F : { x : 'a r } -> 'a ok [@@unboxed]
[%%expect{|
type 'a r = #{ a : 'a; }
and 'a ok = F : { x : 'a r; } -> 'a ok [@@unboxed]
|}]

type 'a r = #{ a : 'a }
type bad = F : 'a r -> bad [@@unboxed]
[%%expect{|
type 'a r = #{ a : 'a; }
Line 2, characters 0-38:
2 | type bad = F : 'a r -> bad [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable "'a".
       You should annotate it with "[@@ocaml.boxed]".
|}]

type 'a r = #{ a : 'a }
type bad = F : { x : 'a r } -> bad [@@unboxed]
[%%expect{|
type 'a r = #{ a : 'a; }
Line 2, characters 0-46:
2 | type bad = F : { x : 'a r } -> bad [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable "'a".
       You should annotate it with "[@@ocaml.boxed]".
|}]

type 'a r = #{ a : 'a }
and 'a r2 = #{ a : 'a r }
and bad = F : 'a r2 -> bad [@@unboxed]
[%%expect{|
Line 3, characters 0-38:
3 | and bad = F : 'a r2 -> bad [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable "'a".
       You should annotate it with "[@@ocaml.boxed]".
|}]

type 'a r = #{ a : 'a }
and bad = F : { x : 'a r } -> bad [@@unboxed]
[%%expect{|
Line 2, characters 0-45:
2 | and bad = F : { x : 'a r } -> bad [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable "'a".
       You should annotate it with "[@@ocaml.boxed]".
|}]

(* #(value & void) and similar kinds are always considered separable,
   since we don't apply the float array optimization for them. *)
type t_void : void
and 'a r = #{ a : 'a ; v : t_void }
and ok = F : 'a r -> ok [@@unboxed]
[%%expect{|
type t_void : void
and 'a r = #{ a : 'a; v : t_void; }
and ok = F : 'a r -> ok [@@unboxed]
|}]

type t_void : void
and 'a r = #{ a : 'a ; v : t_void }
and ok = F : { x : 'a r } -> ok [@@unboxed]
[%%expect{|
type t_void : void
and 'a r = #{ a : 'a; v : t_void; }
and ok = F : { x : 'a r; } -> ok [@@unboxed]
|}]

type t_void : void
and ('a : value) r = #{ a : 'a ; v : t_void }
and ok = F : 'a r -> ok [@@unboxed]
[%%expect{|
type t_void : void
and 'a r = #{ a : 'a; v : t_void; }
and ok = F : 'a r -> ok [@@unboxed]
|}]

(* CR layouts v12: Double-check this is safe when we add [void]. *)
(* CR reisenberg: This is obviously terrible. The workaround is not
   to use [any] in the kind annotation there, when [void] would be
   more accurate. I think fixing this properly will require enhancing
   [Ctype.unbox_once] to look through unboxed records better.
   My local branch [expand-unboxed-records] (in my [fl-kinds] clone)
   has a start to a solution. But I think ccasinghino doesn't like
   it, so we should discuss. *)
type t_void : void
and 'a r : value & any = #{ a : 'a ; v : t_void }
and ok = F : { x : 'a r } -> ok [@@unboxed]
[%%expect{|
type t_void : void
and 'a r = #{ a : 'a; v : t_void; }
and ok = F : { x : 'a r; } -> ok [@@unboxed]
|}]
