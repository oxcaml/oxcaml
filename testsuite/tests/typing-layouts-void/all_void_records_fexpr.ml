(* TEST
 flambda2;
 flags = "-extension layouts_beta -drawfexpr -dno-unique-ids";
 expect.opt;
*)

(* Unboxed all-void records are erased; boxed ones retain an empty block.
   Void field accesses emit no loads or stores. Physical sharing is not
   required. *)
type u = #{ field : unit# }
[%%expect{|

After CPS conversion:
(let $camlTOP1__empty_block_0 = Block 0 () in
 cont k ($camlTOP1__empty_block_0))
  where k define_root_symbol (module_block) =
    let $camlTOP1 = Block 0 () in
    cont done ($camlTOP1)

type u = #{ field : unit#; }
|}]

type ('a : any) t = { mutable field : 'a }
[%%expect{|

After CPS conversion:
(let $camlTOP2__empty_block_1 = Block 0 () in
 cont k ($camlTOP2__empty_block_1))
  where k define_root_symbol (module_block) =
    let $camlTOP2 = Block 0 () in
    cont done ($camlTOP2)

type ('a : any) t = { mutable field : 'a; }
|}]

let unboxed () : u = #{ field = #() }
[%%expect{|

After CPS conversion:
let $camlTOP3__first_const_2 = Block 0 () in
(let code size(1)
       unboxed_0 (param : imm tagged)
         my_closure &my_alloc_region my_depth
         -> k1 * k2
         : unit =
   let next_depth = rec_info (succ my_depth) in
   cont k1
 in
 let unboxed = closure unboxed_0 @unboxed &toplevel.alloc_region in
 let Pmakeblock = %block.[`0`].`toplevel` (unboxed) in
 cont k (Pmakeblock))
  where k define_root_symbol (module_block) =
    let field_0 = %block_load.tag[`0`].`size`[`1`].[`0`] (module_block) in
    let $camlTOP3 = Block 0 (field_0) in
    cont done ($camlTOP3)

val unboxed : unit -> u = <fun>
|}]

let make () : unit# t = { field = #() }
[%%expect{|

After CPS conversion:
(let $camlTOP4__Pmakeblock_5 = Block 0 () in
 let code size(1)
       make_1 (param : imm tagged)
         my_closure &my_alloc_region my_depth
         -> k1 * k2
         : val =
   let next_depth = rec_info (succ my_depth) in
   cont k1 ($camlTOP4__Pmakeblock_5)
 in
 let make = closure make_1 @make &toplevel.alloc_region in
 let Pmakeblock = %block.[`0`].`toplevel` (make) in
 cont k (Pmakeblock))
  where k define_root_symbol (module_block) =
    let field_0 = %block_load.tag[`0`].`size`[`1`].[`0`] (module_block) in
    let $camlTOP4 = Block 0 (field_0) in
    cont done ($camlTOP4)

val make : unit -> unit# t = <fun>
|}]

let product () : #(unit# * unit#) t = { field = #(#(), #()) }
[%%expect{|

After CPS conversion:
(let $camlTOP5__Pmakeblock_8 = Block 0 () in
 let code size(1)
       product_2 (param : imm tagged)
         my_closure &my_alloc_region my_depth
         -> k1 * k2
         : val =
   let next_depth = rec_info (succ my_depth) in
   cont k1 ($camlTOP5__Pmakeblock_8)
 in
 let `product` = closure product_2 @`product` &toplevel.alloc_region in
 let Pmakeblock = %block.[`0`].`toplevel` (`product`) in
 cont k (Pmakeblock))
  where k define_root_symbol (module_block) =
    let field_0 = %block_load.tag[`0`].`size`[`1`].[`0`] (module_block) in
    let $camlTOP5 = Block 0 (field_0) in
    cont done ($camlTOP5)

val product : unit -> #(unit# * unit#) t = <fun>
|}]

let get (t : unit# t) = t.field
[%%expect{|

After CPS conversion:
let $camlTOP6__first_const_11 = Block 0 () in
(let code size(1)
       get_3 (t : val) my_closure &my_alloc_region my_depth -> k1 * k2 : unit =
   let next_depth = rec_info (succ my_depth) in
   cont k1
 in
 let get = closure get_3 @get &toplevel.alloc_region in
 let Pmakeblock = %block.[`0`].`toplevel` (get) in
 cont k (Pmakeblock))
  where k define_root_symbol (module_block) =
    let field_0 = %block_load.tag[`0`].`size`[`1`].[`0`] (module_block) in
    let $camlTOP6 = Block 0 (field_0) in
    cont done ($camlTOP6)

val get : unit# t -> unit# = <fun>
|}]

let set (t : unit# t) = t.field <- #()
[%%expect{|

After CPS conversion:
let $camlTOP7__first_const_14 = Block 0 () in
(let code size(1)
       set_4 (t : val)
         my_closure &my_alloc_region my_depth
         -> k1 * k2
         : imm tagged =
   let next_depth = rec_info (succ my_depth) in
   cont k1 (0)
 in
 let set = closure set_4 @set &toplevel.alloc_region in
 let Pmakeblock = %block.[`0`].`toplevel` (set) in
 cont k (Pmakeblock))
  where k define_root_symbol (module_block) =
    let field_0 = %block_load.tag[`0`].`size`[`1`].[`0`] (module_block) in
    let $camlTOP7 = Block 0 (field_0) in
    cont done ($camlTOP7)

val set : unit# t -> unit = <fun>
|}]
