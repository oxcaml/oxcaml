(* TEST
 flags = "-extension layouts_beta -dlambda -dno-unique-ids";
 expect;
*)

(* All-void records are (1.) erased if unboxed, (2.) atoms otherwise. *)

external unbox_unit : unit -> unit# = "%unbox_unit"
[%%expect{|
0
external unbox_unit : unit -> unit# = "%unbox_unit"
|}]

(* Unboxed: erased. *)

type all_void_unboxed = #{ field : unit# }
[%%expect{|
0
type all_void_unboxed = #{ field : unit#; }
|}]

let mk_u () : all_void_unboxed = #{ field = unbox_unit () }
[%%expect{|
(let (mk_u = (function {nlocal = 0} param[value<int>] : #() (unbox_unit 0)))
  (apply (field_imm 1 (global Toploop!)) "mk_u" mk_u))
val mk_u : unit -> all_void_unboxed = <fun>
|}]

let proj_u (u : all_void_unboxed) = u.#field
[%%expect{|
(let (proj_u = (function {nlocal = 0} u[#()] : #() u))
  (apply (field_imm 1 (global Toploop!)) "proj_u" proj_u))
val proj_u : all_void_unboxed -> unit# = <fun>
|}]

(* Boxed: atoms.
   Atoms ought to be statically allocated, so output must not contain
   [makeblock], [setfield], [field], etc. *)

type all_void_value = { field : unit# }
[%%expect{|
0
type all_void_value = { field : unit#; }
|}]

let mk_v () : all_void_value = { field = unbox_unit () }
[%%expect{|
TODO
|}]

let proj_v (t : all_void_value) = t.field
[%%expect{|
TODO
|}]

(* An all-void product field is void for representation purposes: this must
   generate the same code as [all_void_value]. *)

type all_void_product = { field : #(unit# * unit#) }
[%%expect{|
0
type all_void_product = { field : #(unit# * unit#); }
|}]

let mk_p () : all_void_product =
  { field = #(unbox_unit (), unbox_unit ()) }
[%%expect{|
TODO
|}]

let proj_p (t : all_void_product) = t.field
[%%expect{|
TODO
|}]

(* Mutation evaluates the new value and performs no store. *)

type all_void_mutable = { mutable field : unit# }
[%%expect{|
0
type all_void_mutable = { mutable field : unit#; }
|}]

let set (t : all_void_mutable) = t.field <- unbox_unit ()
[%%expect{|
TODO
|}]

(* All-void records obtained by instantiating `any`
   ought to match corresponding definitions above. *)

type ('a : any) generic_any = { field : 'a }
[%%expect{|
0
type ('a : any) generic_any = { field : 'a; }
|}]

type all_void_value_any = unit# generic_any
[%%expect{|
0
type all_void_value_any = unit# generic_any
|}]

let mk_v_any () : all_void_value_any = { field = unbox_unit () }
[%%expect{|
TODO
|}]

let proj_v_any (t : all_void_value_any) = t.field
[%%expect{|
TODO
|}]

type all_void_product_any = #(unit# * unit#) generic_any
[%%expect{|
0
type all_void_product_any = #(unit# * unit#) generic_any
|}]

let mk_p_any () : all_void_product_any =
  { field = #(unbox_unit (), unbox_unit ()) }
[%%expect{|
TODO
|}]

let proj_p_any (t : all_void_product_any) = t.field
[%%expect{|
TODO
|}]

(* Same with [mutable] [any]: *)

type ('a : any) generic_any_mutable = { mutable field : 'a }
[%%expect{|
0
type ('a : any) generic_any_mutable = { mutable field : 'a; }
|}]

type all_void_mutable_any = unit# generic_any_mutable
[%%expect{|
0
type all_void_mutable_any = unit# generic_any_mutable
|}]

let set_any (t : all_void_mutable_any) = t.field <- unbox_unit ()
[%%expect{|
TODO
|}]

(* Implicit unboxed versions of all-void records: *)

type all_void_value_u : void = all_void_value#
[%%expect{|
0
type all_void_value_u = all_void_value#
|}]
