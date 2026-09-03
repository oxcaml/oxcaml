(* TEST
 flambda2;
 {
   flags = "-extension layouts_beta";
   expect.opt;
 }{
   flags = "-extension layouts_beta -Oclassic";
   expect.opt;
 }{
   flags = "-extension layouts_beta -O3";
   expect.opt;
 }
*)

(* In native code, all-[void] records must be represented as atoms.
   Behavior shared with bytecode (notably *not* memory representation)
   is tested in [all_void_records_runtime.ml]. *)

let describe x =
  let o = Obj.repr x in
  if Obj.is_int o
  then Printf.sprintf "imm %d" (Obj.obj o : int)
  else Printf.sprintf "block tag %d size %d" (Obj.tag o) (Obj.size o)
[%%expect{|
val describe : 'a -> string = <fun>
|}]

type t = { x : unit# }
type p = { y : #(unit# * unit#) }
type m = { mutable z : unit# }
[%%expect{|
type t = { x : unit#; }
type p = { y : #(unit# * unit#); }
type m = { mutable z : unit#; }
|}]

(* Representation: an empty tag-0 block. *)

let reprs =
  [describe { x = #() }; describe { y = #(#(), #()) }; describe { z = #() }]
[%%expect{|
val reprs : string list =
  ["block tag 0 size 0"; "block tag 0 size 0"; "block tag 0 size 0"]
|}]

(* No allocation: constructing in a loop must not move the minor heap. *)

let no_alloc =
  let before = Gc.minor_words () in
  for _ = 1 to 1000 do
    ignore (Sys.opaque_identity { x = #() })
  done;
  Gc.minor_words () -. before = 0.
[%%expect{|
val no_alloc : bool = true
|}]

(* All such records are the atom, so they test as equal w.r.t. [==]. *)

let phys_equal = { x = #() } == { x = #() }
[%%expect{|
val phys_equal : bool = true
|}]

let phys_equal_mutable = { z = #() } == { z = #() }
[%%expect{|
val phys_equal_mutable : bool = true
|}]

(* Functional update ([Pduprecord]) copies the block's real size,
   not one word per label; a copy of the atom is the atom. *)

let[@warning "-23"] functional_update_size =
  let r = { x = #() } in
  describe { r with x = #() }
[%%expect{|
val functional_update_size : string = "block tag 0 size 0"
|}]

let[@warning "-23"] functional_update_identity =
  let r = { x = #() } in
  { r with x = #() } == r
[%%expect{|
val functional_update_identity : bool = true
|}]

(* The atom is an ordinary scanned value when stored in other blocks. *)

let nested =
  [describe (Some { x = #() }); describe [{ x = #() }; { x = #() }]]
[%%expect{|
val nested : string list = ["block tag 0 size 1"; "block tag 0 size 2"]
|}]

(* Recursive bindings: the pre-allocated dummy, if any, has size 0. *)

let letrec_reprs =
  let rec r = { x = #() }
  and f () = r in
  [describe r; describe (f ())]
[%%expect{|
val letrec_reprs : string list = ["block tag 0 size 0"; "block tag 0 size 0"]
|}]

(* Void fields never count towards the size of a block,
   whether or not the record is all-void;
   in particular, functional update must copy
   the block's real size, not one word per label. *)

type tv = { i : int; v : unit# }
[%%expect{|
type tv = { i : int; v : unit#; }
|}]

let void_field_sizes =
  let r = { i = 1; v = #() } in
  [describe r; describe { r with i = 2 }]
[%%expect{|
val void_field_sizes : string list = ["block tag 0 size 1"; "block tag 0 size 1"]
|}]

(* Optimization: an empty record flowing through
   join points, unboxable parameters, and inlined returns
   must not be re-allocated. *)

(* Join point: the same variable bound on two branches. *)
let[@inline never] join b =
  let r = if b then { x = #() } else { x = #() } in
  describe r
let join_result = [join true; join false]
[%%expect{|
val join : bool -> string = <fun>
val join_result : string list = ["block tag 0 size 0"; "block tag 0 size 0"]
|}]

(* Loop parameter: a candidate for continuation-parameter unboxing.
   If the loop rebuilt the record, the result would not be the original. *)
let loop_result =
  let rec go n (r : t) = if n = 0 then r else go (n - 1) r in
  let r = { x = #() } in
  let r' = go 1000 r in
  describe r', r' == r
[%%expect{|
val loop_result : string * bool = ("block tag 0 size 0", true)
|}]

(* Inlined return: the record is constructed in the callee and
   used in the caller after inlining. *)
let[@inline always] mk () = { x = #() }
let inlined_result = describe (mk ())
let inlined_phys_equal = mk () == mk ()
[%%expect{|
val mk : unit -> t = <fun>
val inlined_result : string = "block tag 0 size 0"
val inlined_phys_equal : bool = true
|}]

(* All-void records obtained by instantiating [any] with a [void] type
   must be represented exactly like the directly-declared ones above. *)

type ('a : any) r = { x : 'a }
[%%expect{|
type ('a : any) r = { x : 'a; }
|}]

let any_reprs =
  [ describe ({ x = #() } : unit# r);
    describe ({ x = #(#(), #()) } : #(unit# * unit#) r);
    describe ({ x = 3 } : int r) ]
[%%expect{|
val any_reprs : string list =
  ["block tag 0 size 0"; "block tag 0 size 0"; "block tag 0 size 1"]
|}]

let any_phys_equal = ({ x = #() } : unit# r) == { x = #() }
[%%expect{|
val any_phys_equal : bool = true
|}]
