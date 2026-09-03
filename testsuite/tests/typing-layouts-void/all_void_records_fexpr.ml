(* TEST
 flags = "-extension layouts_beta -drawfexpr -dno-unique-ids";
 expect.opt;
*)

(* All-void records are erased if unboxed.
   Otherwise, unarization deletes their fields, so the raw Flambda2 output
   should represent them as static, empty tag-0 blocks. Their physical
   identity is unspecified: they need not share a block or use an atom. *)

external unbox_unit : unit -> unit# = "%unbox_unit"
[%%expect{|

After CPS conversion:
(let $camlTOP1__empty_block_0 = Block 0 () in
 cont k ($camlTOP1__empty_block_0))
  where k define_root_symbol (module_block) =
    let $camlTOP1 = Block 0 () in
    cont done ($camlTOP1)

external unbox_unit : unit -> unit# = "%unbox_unit"
|}]

(* Unboxed: erased. *)

type all_void_unboxed = #{ field : unit# }
[%%expect{|

After CPS conversion:
(let $camlTOP2__empty_block_1 = Block 0 () in
 cont k ($camlTOP2__empty_block_1))
  where k define_root_symbol (module_block) =
    let $camlTOP2 = Block 0 () in
    cont done ($camlTOP2)

type all_void_unboxed = #{ field : unit#; }
|}]

let mk_u () : all_void_unboxed = #{ field = unbox_unit () }
[%%expect{|

After CPS conversion:
let $camlTOP3__first_const_2 = Block 0 () in
(let code size(1)
       mk_u_0 (param : imm tagged)
         my_closure &my_alloc_region my_depth
         -> k1 * k2
         : unit =
   let next_depth = rec_info (succ my_depth) in
   cont k1
 in
 let mk_u = closure mk_u_0 @mk_u &toplevel.alloc_region in
 let Pmakeblock = %block.[`0`].`toplevel` (mk_u) in
 cont k (Pmakeblock))
  where k define_root_symbol (module_block) =
    let field_0 = %block_load.tag[`0`].`size`[`1`].[`0`] (module_block) in
    let $camlTOP3 = Block 0 (field_0) in
    cont done ($camlTOP3)

val mk_u : unit -> all_void_unboxed = <fun>
|}]

let proj_u (u : all_void_unboxed) = u.#field
[%%expect{|

After CPS conversion:
let $camlTOP4__first_const_5 = Block 0 () in
(let code size(1)
       proj_u_1 my_closure &my_alloc_region my_depth -> k1 * k2 : unit =
   let next_depth = rec_info (succ my_depth) in
   cont k1
 in
 let proj_u = closure proj_u_1 @proj_u &toplevel.alloc_region in
 let Pmakeblock = %block.[`0`].`toplevel` (proj_u) in
 cont k (Pmakeblock))
  where k define_root_symbol (module_block) =
    let field_0 = %block_load.tag[`0`].`size`[`1`].[`0`] (module_block) in
    let $camlTOP4 = Block 0 (field_0) in
    cont done ($camlTOP4)

val proj_u : all_void_unboxed -> unit# = <fun>
|}]

(* Boxed: a mixed block with a void shape. *)

type all_void_value = { field : unit# }
[%%expect{|

After CPS conversion:
(let $camlTOP5__empty_block_8 = Block 0 () in
 cont k ($camlTOP5__empty_block_8))
  where k define_root_symbol (module_block) =
    let $camlTOP5 = Block 0 () in
    cont done ($camlTOP5)

type all_void_value = { field : unit#; }
|}]

let mk_v () : all_void_value = { field = unbox_unit () }
[%%expect{|

After CPS conversion:
(let $camlTOP6__Pmakeblock_9 = Block 0 () in
 let code size(1)
       mk_v_2 (param : imm tagged)
         my_closure &my_alloc_region my_depth
         -> k1 * k2
         : [ 0 of  ] =
   let next_depth = rec_info (succ my_depth) in
   cont k1 ($camlTOP6__Pmakeblock_9)
 in
 let mk_v = closure mk_v_2 @mk_v &toplevel.alloc_region in
 let Pmakeblock = %block.[`0`].`toplevel` (mk_v) in
 cont k (Pmakeblock))
  where k define_root_symbol (module_block) =
    let field_0 = %block_load.tag[`0`].`size`[`1`].[`0`] (module_block) in
    let $camlTOP6 = Block 0 (field_0) in
    cont done ($camlTOP6)

val mk_v : unit -> all_void_value = <fun>
|}]

let proj_v (t : all_void_value) = t.field
[%%expect{|

After CPS conversion:
let $camlTOP7__first_const_12 = Block 0 () in
(let code size(1)
       proj_v_3 (t : [ 0 of  ])
         my_closure &my_alloc_region my_depth
         -> k1 * k2
         : unit =
   let next_depth = rec_info (succ my_depth) in
   cont k1
 in
 let proj_v = closure proj_v_3 @proj_v &toplevel.alloc_region in
 let Pmakeblock = %block.[`0`].`toplevel` (proj_v) in
 cont k (Pmakeblock))
  where k define_root_symbol (module_block) =
    let field_0 = %block_load.tag[`0`].`size`[`1`].[`0`] (module_block) in
    let $camlTOP7 = Block 0 (field_0) in
    cont done ($camlTOP7)

val proj_v : all_void_value -> unit# = <fun>
|}]

(* An all-void product field is void for representation purposes. The shape
   differs from [all_void_value]'s before unarization, but flattens to the
   same empty-block shape. This does not require physical sharing. *)

type all_void_product = { field : #(unit# * unit#) }
[%%expect{|

After CPS conversion:
(let $camlTOP8__empty_block_15 = Block 0 () in
 cont k ($camlTOP8__empty_block_15))
  where k define_root_symbol (module_block) =
    let $camlTOP8 = Block 0 () in
    cont done ($camlTOP8)

type all_void_product = { field : #(unit# * unit#); }
|}]

let mk_p () : all_void_product =
  { field = #(unbox_unit (), unbox_unit ()) }
[%%expect{|

After CPS conversion:
(let $camlTOP9__Pmakeblock_16 = Block 0 () in
 let code size(1)
       mk_p_4 (param : imm tagged)
         my_closure &my_alloc_region my_depth
         -> k1 * k2
         : [ 0 of  ] =
   let next_depth = rec_info (succ my_depth) in
   cont k1 ($camlTOP9__Pmakeblock_16)
 in
 let mk_p = closure mk_p_4 @mk_p &toplevel.alloc_region in
 let Pmakeblock = %block.[`0`].`toplevel` (mk_p) in
 cont k (Pmakeblock))
  where k define_root_symbol (module_block) =
    let field_0 = %block_load.tag[`0`].`size`[`1`].[`0`] (module_block) in
    let $camlTOP9 = Block 0 (field_0) in
    cont done ($camlTOP9)

val mk_p : unit -> all_void_product = <fun>
|}]

let proj_p (t : all_void_product) = t.field
[%%expect{|

After CPS conversion:
let $camlTOP10__first_const_19 = Block 0 () in
(let code size(1)
       proj_p_5 (t : [ 0 of  ])
         my_closure &my_alloc_region my_depth
         -> k1 * k2
         : unit =
   let next_depth = rec_info (succ my_depth) in
   cont k1
 in
 let proj_p = closure proj_p_5 @proj_p &toplevel.alloc_region in
 let Pmakeblock = %block.[`0`].`toplevel` (proj_p) in
 cont k (Pmakeblock))
  where k define_root_symbol (module_block) =
    let field_0 = %block_load.tag[`0`].`size`[`1`].[`0`] (module_block) in
    let $camlTOP10 = Block 0 (field_0) in
    cont done ($camlTOP10)

val proj_p : all_void_product -> #(unit# * unit#) = <fun>
|}]

(* Mutation: a [setmixedfield] of a void, which unarizes to no store. *)

type all_void_mutable = { mutable field : unit# }
[%%expect{|

After CPS conversion:
(let $camlTOP11__empty_block_22 = Block 0 () in
 cont k ($camlTOP11__empty_block_22))
  where k define_root_symbol (module_block) =
    let $camlTOP11 = Block 0 () in
    cont done ($camlTOP11)

type all_void_mutable = { mutable field : unit#; }
|}]

let set (t : all_void_mutable) = t.field <- unbox_unit ()
[%%expect{|

After CPS conversion:
let $camlTOP12__first_const_23 = Block 0 () in
(let code size(1)
       set_6 (t : val)
         my_closure &my_alloc_region my_depth
         -> k1 * k2
         : imm tagged =
   let next_depth = rec_info (succ my_depth) in
   cont k1 (0)
 in
 let set = closure set_6 @set &toplevel.alloc_region in
 let Pmakeblock = %block.[`0`].`toplevel` (set) in
 cont k (Pmakeblock))
  where k define_root_symbol (module_block) =
    let field_0 = %block_load.tag[`0`].`size`[`1`].[`0`] (module_block) in
    let $camlTOP12 = Block 0 (field_0) in
    cont done ($camlTOP12)

val set : all_void_mutable -> unit = <fun>
|}]

(* All-void records obtained by instantiating [any] must have the same
   representation as the corresponding definitions above. This does not
   require identical IR annotations or physical sharing. *)

type ('a : any) generic_any = { field : 'a }
[%%expect{|

After CPS conversion:
(let $camlTOP13__empty_block_26 = Block 0 () in
 cont k ($camlTOP13__empty_block_26))
  where k define_root_symbol (module_block) =
    let $camlTOP13 = Block 0 () in
    cont done ($camlTOP13)

type ('a : any) generic_any = { field : 'a; }
|}]

type all_void_value_any = unit# generic_any
[%%expect{|

After CPS conversion:
(let $camlTOP14__empty_block_27 = Block 0 () in
 cont k ($camlTOP14__empty_block_27))
  where k define_root_symbol (module_block) =
    let $camlTOP14 = Block 0 () in
    cont done ($camlTOP14)

type all_void_value_any = unit# generic_any
|}]

let mk_v_any () : all_void_value_any = { field = unbox_unit () }
[%%expect{|

After CPS conversion:
(let $camlTOP15__Pmakeblock_28 = Block 0 () in
 let code size(1)
       mk_v_any_7 (param : imm tagged)
         my_closure &my_alloc_region my_depth
         -> k1 * k2
         : val =
   let next_depth = rec_info (succ my_depth) in
   cont k1 ($camlTOP15__Pmakeblock_28)
 in
 let mk_v_any = closure mk_v_any_7 @mk_v_any &toplevel.alloc_region in
 let Pmakeblock = %block.[`0`].`toplevel` (mk_v_any) in
 cont k (Pmakeblock))
  where k define_root_symbol (module_block) =
    let field_0 = %block_load.tag[`0`].`size`[`1`].[`0`] (module_block) in
    let $camlTOP15 = Block 0 (field_0) in
    cont done ($camlTOP15)

val mk_v_any : unit -> all_void_value_any = <fun>
|}]

let proj_v_any (t : all_void_value_any) = t.field
[%%expect{|

After CPS conversion:
let $camlTOP16__first_const_31 = Block 0 () in
(let code size(1)
       proj_v_any_8 (t : val)
         my_closure &my_alloc_region my_depth
         -> k1 * k2
         : unit =
   let next_depth = rec_info (succ my_depth) in
   cont k1
 in
 let proj_v_any = closure proj_v_any_8 @proj_v_any &toplevel.alloc_region in
 let Pmakeblock = %block.[`0`].`toplevel` (proj_v_any) in
 cont k (Pmakeblock))
  where k define_root_symbol (module_block) =
    let field_0 = %block_load.tag[`0`].`size`[`1`].[`0`] (module_block) in
    let $camlTOP16 = Block 0 (field_0) in
    cont done ($camlTOP16)

val proj_v_any : all_void_value_any -> unit# = <fun>
|}]

type all_void_product_any = #(unit# * unit#) generic_any
[%%expect{|

After CPS conversion:
(let $camlTOP17__empty_block_34 = Block 0 () in
 cont k ($camlTOP17__empty_block_34))
  where k define_root_symbol (module_block) =
    let $camlTOP17 = Block 0 () in
    cont done ($camlTOP17)

type all_void_product_any = #(unit# * unit#) generic_any
|}]

let mk_p_any () : all_void_product_any =
  { field = #(unbox_unit (), unbox_unit ()) }
[%%expect{|

After CPS conversion:
(let $camlTOP18__Pmakeblock_35 = Block 0 () in
 let code size(1)
       mk_p_any_9 (param : imm tagged)
         my_closure &my_alloc_region my_depth
         -> k1 * k2
         : val =
   let next_depth = rec_info (succ my_depth) in
   cont k1 ($camlTOP18__Pmakeblock_35)
 in
 let mk_p_any = closure mk_p_any_9 @mk_p_any &toplevel.alloc_region in
 let Pmakeblock = %block.[`0`].`toplevel` (mk_p_any) in
 cont k (Pmakeblock))
  where k define_root_symbol (module_block) =
    let field_0 = %block_load.tag[`0`].`size`[`1`].[`0`] (module_block) in
    let $camlTOP18 = Block 0 (field_0) in
    cont done ($camlTOP18)

val mk_p_any : unit -> all_void_product_any = <fun>
|}]

let proj_p_any (t : all_void_product_any) = t.field
[%%expect{|

After CPS conversion:
let $camlTOP19__first_const_38 = Block 0 () in
(let code size(1)
       proj_p_any_10 (t : val)
         my_closure &my_alloc_region my_depth
         -> k1 * k2
         : unit =
   let next_depth = rec_info (succ my_depth) in
   cont k1
 in
 let proj_p_any = closure proj_p_any_10 @proj_p_any &toplevel.alloc_region in
 let Pmakeblock = %block.[`0`].`toplevel` (proj_p_any) in
 cont k (Pmakeblock))
  where k define_root_symbol (module_block) =
    let field_0 = %block_load.tag[`0`].`size`[`1`].[`0`] (module_block) in
    let $camlTOP19 = Block 0 (field_0) in
    cont done ($camlTOP19)

val proj_p_any : all_void_product_any -> #(unit# * unit#) = <fun>
|}]

(* Same with [mutable] [any]: *)

type ('a : any) generic_any_mutable = { mutable field : 'a }
[%%expect{|

After CPS conversion:
(let $camlTOP20__empty_block_41 = Block 0 () in
 cont k ($camlTOP20__empty_block_41))
  where k define_root_symbol (module_block) =
    let $camlTOP20 = Block 0 () in
    cont done ($camlTOP20)

type ('a : any) generic_any_mutable = { mutable field : 'a; }
|}]

type all_void_mutable_any = unit# generic_any_mutable
[%%expect{|

After CPS conversion:
(let $camlTOP21__empty_block_42 = Block 0 () in
 cont k ($camlTOP21__empty_block_42))
  where k define_root_symbol (module_block) =
    let $camlTOP21 = Block 0 () in
    cont done ($camlTOP21)

type all_void_mutable_any = unit# generic_any_mutable
|}]

let set_any (t : all_void_mutable_any) = t.field <- unbox_unit ()
[%%expect{|

After CPS conversion:
let $camlTOP22__first_const_43 = Block 0 () in
(let code size(1)
       set_any_11 (t : val)
         my_closure &my_alloc_region my_depth
         -> k1 * k2
         : imm tagged =
   let next_depth = rec_info (succ my_depth) in
   cont k1 (0)
 in
 let set_any = closure set_any_11 @set_any &toplevel.alloc_region in
 let Pmakeblock = %block.[`0`].`toplevel` (set_any) in
 cont k (Pmakeblock))
  where k define_root_symbol (module_block) =
    let field_0 = %block_load.tag[`0`].`size`[`1`].[`0`] (module_block) in
    let $camlTOP22 = Block 0 (field_0) in
    cont done ($camlTOP22)

val set_any : all_void_mutable_any -> unit = <fun>
|}]

(* Implicit unboxed versions of all-void records: *)

type all_void_value_u : void = all_void_value#
[%%expect{|

After CPS conversion:
(let $camlTOP23__empty_block_46 = Block 0 () in
 cont k ($camlTOP23__empty_block_46))
  where k define_root_symbol (module_block) =
    let $camlTOP23 = Block 0 () in
    cont done ($camlTOP23)

type all_void_value_u = all_void_value#
|}]
