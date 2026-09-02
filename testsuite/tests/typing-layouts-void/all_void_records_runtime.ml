(* TEST
 flambda2;
 {
   expect;
 }{
   expect.opt;
 }{
   flags = "-Oclassic";
   expect.opt;
 }{
   flags = "-O3";
   expect.opt;
 }
*)

(* All-void records are (1.) erased if unboxed, (2.) atoms otherwise. *)

let describe x =
  let o = Obj.repr x in
  if Obj.is_int o
  then Printf.sprintf "imm %d" (Obj.obj o : int)
  else Printf.sprintf "block tag %d size %d" (Obj.tag o) (Obj.size o)
[%%expect{|
val describe : 'a -> string = <fun>
|}]

type t = { x : unit# }
[%%expect{|
type t = { x : unit#; }
|}]

(* A field whose layout is an all-void product is still void for
   representation purposes. *)
type p = { y : #(unit# * unit#) }
[%%expect{|
type p = { y : #(unit# * unit#); }
|}]

type m = { mutable z : unit# }
[%%expect{|
type m = { mutable z : unit#; }
|}]

(* Representation: the tag-0 atom. *)

let reprs =
  [describe { x = #() }; describe { y = #(#(), #()) }; describe { z = #() }]
[%%expect{|
val reprs : string list =
  ["block tag 0 size 0"; "block tag 0 size 0"; "block tag 0 size 0"]
|}]

(* No allocation: every construction yields the same static atom. Constructing
   in a loop must not move the minor heap. *)

let no_alloc =
  let before = Gc.minor_words () in
  for _ = 1 to 1000 do
    ignore (Sys.opaque_identity { x = #() })
  done;
  Gc.minor_words () -. before = 0.
[%%expect{|
val no_alloc : bool = true
|}]

(* A consequence of representing all-void records with atoms
   is that all such records test as equal w.r.t. [==]: *)

let phys_equal = { x = #() } == { x = #() }
[%%expect{|
val phys_equal : bool = true
|}]

let phys_equal_mutable = { z = #() } == { z = #() }
[%%expect{|
val phys_equal_mutable : bool = true
|}]

(* Projection and matching work (vacuously). *)

let proj (r : t) = let #() = r.x in "projected"
let proj_result = proj { x = #() }
[%%expect{|
val proj : t -> string = <fun>
val proj_result : string = "projected"
|}]

let match_result =
  match { y = #(#(), #()) } with
  | { y = #(v, _) } -> let #() = v in "matched"
[%%expect{|
val match_result : string = "matched"
|}]

(* Field expressions are still evaluated in order,
   even though nothing is stored. *)

let field_effects =
  let log = ref [] in
  let eff s = log := s :: !log; #() in
  let r = { x = eff "x" } in
  let _ : t = r in
  let p = { y = #(eff "y1", eff "y2") } in
  let _ : p = p in
  List.rev !log
[%%expect{|
val field_effects : string list = ["x"; "y2"; "y1"]
|}]

(* The record expression of a projection is evaluated for its effects. *)

let proj_effects =
  let log = ref [] in
  let r () = log := "r" :: !log; { x = #() } in
  let #() = (r ()).x in
  List.rev !log
[%%expect{|
val proj_effects : string list = ["r"]
|}]

(* Mutation: the new value is evaluated, and the store is a no-op. *)

let mutation =
  let log = ref [] in
  let eff s = log := s :: !log; #() in
  let r = { z = #() } in
  r.z <- eff "set";
  let #() = r.z in
  List.rev !log
[%%expect{|
val mutation : string list = ["set"]
|}]

(* Functional update ([Pduprecord]) must not try to copy zero words. *)

let functional_update =
  let r = { x = #() } in
  describe { r with x = #() }
[%%expect{|
val functional_update : string = "block tag 0 size 0"
|}]

(* Polymorphic operations. *)

let structural_equal = ({ x = #() } = { x = #() })
[%%expect{|
val structural_equal : bool = true
|}]

let compare_equal = compare { x = #() } { x = #() }
[%%expect{|
val compare_equal : int = 0
|}]

let hash_equal = Hashtbl.hash { x = #() } = Hashtbl.hash { x = #() }
[%%expect{|
val hash_equal : bool = true
|}]

let marshal_round_trip =
  describe (Marshal.from_string (Marshal.to_string { x = #() } []) 0 : t)
[%%expect{|
val marshal_round_trip : string = "block tag 0 size 0"
|}]

(* The atom is an ordinary scanned value when stored in other blocks. *)

let nested =
  [describe (Some { x = #() }); describe [{ x = #() }; { x = #() }]]
[%%expect{|
val nested : string list = ["block tag 0 size 1"; "block tag 0 size 2"]
|}]

let nested_length = List.length [{ x = #() }; { x = #() }; { x = #() }]
[%%expect{|
val nested_length : int = 3
|}]

(* Recursive bindings. *)

let letrec_reprs =
  let rec r = { x = #() }
  and f () = r in
  [describe r; describe (f ())]
[%%expect{|
val letrec_reprs : string list = ["block tag 0 size 0"; "block tag 0 size 0"]
|}]

let letrec_match =
  let rec r = { x = #() }
  and f () = r in
  let #() = (f ()).x in
  "matched"
[%%expect{|
val letrec_match : string = "matched"
|}]

(* Kinds: an all-void record is a pointer (to the atom), not an immediate. *)

type bad : immediate = { x : unit# }
[%%expect{|
(* CR wsturgeon: promote the expected kind error here *)
|}]
