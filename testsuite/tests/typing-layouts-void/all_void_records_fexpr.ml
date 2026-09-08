(* TEST
 flambda2;
 flags = "-extension layouts_beta -drawfexpr -dno-unique-ids";
 expect.opt;
*)

type ('a : any) t = { mutable field : 'a }
[%%expect{|

After CPS conversion:
(let $camlTOP1__empty_block_0 = Block 0 () in
 cont k ($camlTOP1__empty_block_0))
  where k define_root_symbol (module_block) =
    let $camlTOP1 = Block 0 () in
    cont done ($camlTOP1)

type ('a : any) t = { mutable field : 'a; }
|}]

let make () : unit# t = { field = #() }
[%%expect{|

After CPS conversion:
(let $camlTOP2__Pmakeblock_1 = Block 0 () in
 let code size(1)
       make_0 (param : imm tagged)
         my_closure &my_alloc_region my_depth
         -> k1 * k2
         : val =
   let next_depth = rec_info (succ my_depth) in
   cont k1 ($camlTOP2__Pmakeblock_1)
 in
 let make = closure make_0 @make &toplevel.alloc_region in
 let Pmakeblock = %block.[`0`].`toplevel` (make) in
 cont k (Pmakeblock))
  where k define_root_symbol (module_block) =
    let field_0 = %block_load.tag[`0`].`size`[`1`].[`0`] (module_block) in
    let $camlTOP2 = Block 0 (field_0) in
    cont done ($camlTOP2)

val make : unit -> unit# t = <fun>
|}]

let product () : #(unit# * unit#) t = { field = #(#(), #()) }
[%%expect{|

After CPS conversion:
(let $camlTOP3__Pmakeblock_4 = Block 0 () in
 let code size(1)
       product_1 (param : imm tagged)
         my_closure &my_alloc_region my_depth
         -> k1 * k2
         : val =
   let next_depth = rec_info (succ my_depth) in
   cont k1 ($camlTOP3__Pmakeblock_4)
 in
 let `product` = closure product_1 @`product` &toplevel.alloc_region in
 let Pmakeblock = %block.[`0`].`toplevel` (`product`) in
 cont k (Pmakeblock))
  where k define_root_symbol (module_block) =
    let field_0 = %block_load.tag[`0`].`size`[`1`].[`0`] (module_block) in
    let $camlTOP3 = Block 0 (field_0) in
    cont done ($camlTOP3)

val product : unit -> #(unit# * unit#) t = <fun>
|}]

let get (t : unit# t) = t.field
[%%expect{|

After CPS conversion:
let $camlTOP4__first_const_7 = Block 0 () in
(let code size(1)
       get_2 (t : val) my_closure &my_alloc_region my_depth -> k1 * k2 : unit =
   let next_depth = rec_info (succ my_depth) in
   cont k1
 in
 let get = closure get_2 @get &toplevel.alloc_region in
 let Pmakeblock = %block.[`0`].`toplevel` (get) in
 cont k (Pmakeblock))
  where k define_root_symbol (module_block) =
    let field_0 = %block_load.tag[`0`].`size`[`1`].[`0`] (module_block) in
    let $camlTOP4 = Block 0 (field_0) in
    cont done ($camlTOP4)

val get : unit# t -> unit# = <fun>
|}]

let set (t : unit# t) = t.field <- #()
[%%expect{|

After CPS conversion:
let $camlTOP5__first_const_10 = Block 0 () in
(let code size(1)
       set_3 (t : val)
         my_closure &my_alloc_region my_depth
         -> k1 * k2
         : imm tagged =
   let next_depth = rec_info (succ my_depth) in
   cont k1 (0)
 in
 let set = closure set_3 @set &toplevel.alloc_region in
 let Pmakeblock = %block.[`0`].`toplevel` (set) in
 cont k (Pmakeblock))
  where k define_root_symbol (module_block) =
    let field_0 = %block_load.tag[`0`].`size`[`1`].[`0`] (module_block) in
    let $camlTOP5 = Block 0 (field_0) in
    cont done ($camlTOP5)

val set : unit# t -> unit = <fun>
|}]
