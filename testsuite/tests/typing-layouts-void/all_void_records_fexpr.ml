(* TEST
 flags = "-extension layouts_beta -drawfexpr -dno-unique-ids";
 expect.opt;
*)

(* All-void records are erased if unboxed.
   If not unboxed, unarization will delete each field of an all-void record,
   so `-drawfexpr` above (twin of `dump-raw`, i.e. Flambda2's IR)
   should already use atoms to represent all-void (non-unboxed) records
   (and *not* just zero-size static blocks, since those are not all `==`). *)

external unbox_unit : unit -> unit# = "%unbox_unit"
[%%expect{|
TODO
|}]

(* Unboxed: erased. *)

type all_void_unboxed = #{ field : unit# }
[%%expect{|
TODO
|}]

let mk_u () : all_void_unboxed = #{ field = unbox_unit () }
[%%expect{|
TODO
|}]

let proj_u (u : all_void_unboxed) = u.#field
[%%expect{|
TODO
|}]

(* Boxed: a mixed block with a void shape. *)

type all_void_value = { field : unit# }
[%%expect{|
TODO
|}]

let mk_v () : all_void_value = { field = unbox_unit () }
[%%expect{|
TODO
|}]

let proj_v (t : all_void_value) = t.field
[%%expect{|
TODO
|}]

(* An all-void product field is void for representation purposes. The shape
   differs from [all_void_value]'s at this level, but flattens to the same
   empty block after unarization. *)

type all_void_product = { field : #(unit# * unit#) }
[%%expect{|
TODO
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

(* Mutation: a [setmixedfield] of a void, which unarizes to no store. *)

type all_void_mutable = { mutable field : unit# }
[%%expect{|
TODO
|}]

let set (t : all_void_mutable) = t.field <- unbox_unit ()
[%%expect{|
TODO
|}]

(* All-void records obtained by instantiating [any] ought to generate the same
   code as the corresponding definitions above. *)

type ('a : any) generic_any = { field : 'a }
[%%expect{|
TODO
|}]

type all_void_value_any = unit# generic_any
[%%expect{|
TODO
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
TODO
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
TODO
|}]

type all_void_mutable_any = unit# generic_any_mutable
[%%expect{|
TODO
|}]

let set_any (t : all_void_mutable_any) = t.field <- unbox_unit ()
[%%expect{|
TODO
|}]

(* Implicit unboxed versions of all-void records: *)

type all_void_value_u : void = all_void_value#
[%%expect{|
TODO
|}]
