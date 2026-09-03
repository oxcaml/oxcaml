(* TEST
 flags = "-extension layouts_beta -dlambda -dno-unique-ids";
 expect;
*)

(* All-void records are erased if unboxed.
   If not unboxed, unarization will delete each field of an all-void record.
   Since unarization happens after the lambda pass, all-void records
   should still appear as [makeblock], [mixedfield], etc., here.
   See [all_void_records_native.ml] for their final native representation. *)

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

(* Boxed: a mixed block with a void shape. *)

type all_void_value = { field : unit# }
[%%expect{|
0
type all_void_value = { field : unit#; }
|}]

let mk_v () : all_void_value = { field = unbox_unit () }
[%%expect{|
(let
  (mk_v =
     (function {nlocal = 0} param[value<int>]
       : (consts ()) (non_consts ([0: product ]))
       (makeblock 0 (product ) (unbox_unit 0))))
  (apply (field_imm 1 (global Toploop!)) "mk_v" mk_v))
val mk_v : unit -> all_void_value = <fun>
|}]

let proj_v (t : all_void_value) = t.field
[%%expect{|
(let
  (proj_v =
     (function {nlocal = 0}
       t[value<(consts ()) (non_consts ([0: product ]))>] : #()
       (mixedfield 0  (product ) t)))
  (apply (field_imm 1 (global Toploop!)) "proj_v" proj_v))
val proj_v : all_void_value -> unit# = <fun>
|}]

(* An all-void product field is void for representation purposes. The shape
   differs from [all_void_value]'s at this level, but flattens to the same
   empty block after unarization. *)

type all_void_product = { field : #(unit# * unit#) }
[%%expect{|
0
type all_void_product = { field : #(unit# * unit#); }
|}]

let mk_p () : all_void_product =
  { field = #(unbox_unit (), unbox_unit ()) }
[%%expect{|
(let
  (mk_p =
     (function {nlocal = 0} param[value<int>]
       : (consts ()) (non_consts ([0: product product , product ]))
       (makeblock 0 (product  (product ,product ))
         (make_unboxed_product #(#(), #()) (unbox_unit 0) (unbox_unit 0)))))
  (apply (field_imm 1 (global Toploop!)) "mk_p" mk_p))
val mk_p : unit -> all_void_product = <fun>
|}]

let proj_p (t : all_void_product) = t.field
[%%expect{|
(let
  (proj_p =
     (function {nlocal = 0}
       t[value<(consts ()) (non_consts ([0: product product , product ]))>]
       : #(#(), #()) (mixedfield 0  (product  (product ,product )) t)))
  (apply (field_imm 1 (global Toploop!)) "proj_p" proj_p))
val proj_p : all_void_product -> #(unit# * unit#) = <fun>
|}]

(* Mutation: a [setmixedfield] of a void, which unarizes to no store. *)

type all_void_mutable = { mutable field : unit# }
[%%expect{|
0
type all_void_mutable = { mutable field : unit#; }
|}]

let set (t : all_void_mutable) = t.field <- unbox_unit ()
[%%expect{|
(let
  (set =
     (function {nlocal = 0} t : int
       (setmixedfield 0  (product ) t (unbox_unit 0))))
  (apply (field_imm 1 (global Toploop!)) "set" set))
val set : all_void_mutable -> unit = <fun>
|}]

(* All-void records obtained by instantiating [any] ought to generate the same
   code as the corresponding definitions above. *)

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
(let
  (mk_v_any =
     (function {nlocal = 0} param[value<int>]
       (makeblock 0 (product ) (unbox_unit 0))))
  (apply (field_imm 1 (global Toploop!)) "mk_v_any" mk_v_any))
val mk_v_any : unit -> all_void_value_any = <fun>
|}]

let proj_v_any (t : all_void_value_any) = t.field
[%%expect{|
(let
  (proj_v_any = (function {nlocal = 0} t : #() (mixedfield 0  (product ) t)))
  (apply (field_imm 1 (global Toploop!)) "proj_v_any" proj_v_any))
val proj_v_any : all_void_value_any -> unit# = <fun>
|}]

type all_void_product_any = #(unit# * unit#) generic_any
[%%expect{|
0
type all_void_product_any = #(unit# * unit#) generic_any
|}]

let mk_p_any () : all_void_product_any =
  { field = #(unbox_unit (), unbox_unit ()) }
[%%expect{|
(let
  (mk_p_any =
     (function {nlocal = 0} param[value<int>]
       (makeblock 0 (product  (product ,product ))
         (make_unboxed_product #(#(), #()) (unbox_unit 0) (unbox_unit 0)))))
  (apply (field_imm 1 (global Toploop!)) "mk_p_any" mk_p_any))
val mk_p_any : unit -> all_void_product_any = <fun>
|}]

let proj_p_any (t : all_void_product_any) = t.field
[%%expect{|
(let
  (proj_p_any =
     (function {nlocal = 0} t : #(#(), #())
       (mixedfield 0  (product  (product ,product )) t)))
  (apply (field_imm 1 (global Toploop!)) "proj_p_any" proj_p_any))
val proj_p_any : all_void_product_any -> #(unit# * unit#) = <fun>
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
(let
  (set_any =
     (function {nlocal = 0} t : int
       (setmixedfield 0  (product ) t (unbox_unit 0))))
  (apply (field_imm 1 (global Toploop!)) "set_any" set_any))
val set_any : all_void_mutable_any -> unit = <fun>
|}]

(* Implicit unboxed versions of all-void records: *)

type all_void_value_u : void = all_void_value#
[%%expect{|
0
type all_void_value_u = all_void_value#
|}]
