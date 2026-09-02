(* TEST
 flambda2;
 {
   flags = "-extension layouts_beta";
   { expect; expect.opt; }
 }{
   flags = "-extension layouts_beta -Oclassic";
   expect.opt;
 }{
   flags = "-extension layouts_beta -O3";
   expect.opt;
 }
*)

(* All-void records created by instantiating [any] with a [void] type
   must act as if they were defined directly, without [any]: that is,
   all-void records are (1.) erased if unboxed; (2.) atoms otherwise. *)

let describe x =
  let o = Obj.repr x in
  if Obj.is_int o
  then Printf.sprintf "imm %d" (Obj.obj o : int)
  else Printf.sprintf "block tag %d size %d" (Obj.tag o) (Obj.size o)
[%%expect{|
val describe : 'a -> string = <fun>
|}]

type ('a : any) r = { x : 'a }
[%%expect{|
type ('a : any) r = { x : 'a; }
|}]

let reprs =
  [ describe ({ x = #() } : unit# r);
    describe ({ x = #(#(), #()) } : #(unit# * unit#) r);
    describe ({ x = 3 } : int r) ]
[%%expect{|
val reprs : string list =
  ["block tag 0 size 0"; "block tag 0 size 0"; "block tag 0 size 1"]
|}]

(* A consequence of representing all-void records with atoms
   is that all such records test as equal w.r.t. [==]: *)
let phys_equal = ({ x = #() } : unit# r) == { x = #() }
[%%expect{|
val phys_equal : bool = true
|}]

let proj_result =
  let r : unit# r = { x = #() } in
  let #() = r.x in
  "projected"
[%%expect{|
val proj_result : string = "projected"
|}]

let match_result =
  match ({ x = #(#(), #()) } : #(unit# * unit#) r) with
  | { x = #(v, _) } -> let #() = v in "matched"
[%%expect{|
val match_result : string = "matched"
|}]
