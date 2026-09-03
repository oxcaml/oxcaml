(* TEST
 flambda2;
 include stdlib_stable;
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

(* This file tests all-void record behavior that must be
   shared between bytecode and native code.
   Native-only traits (e.g. memory representation)
   are tested in [all_void_records_native.ml]. *)

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

(* Mutation: the new value is evaluated. *)

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

(* Block indices, both reading and writing. *)

let idx_round_trip =
  let r = { z = #() } in
  Stdlib_stable.Idx_mut.set r (.z) #();
  let #() = Stdlib_stable.Idx_mut.get r (.z) in
  "indexed"
[%%expect{|
val idx_round_trip : string = "indexed"
|}]

(* Functional update. *)

let[@warning "-23"] functional_update =
  let r = { x = #() } in
  let #() = { r with x = #() }.x in
  "updated"
[%%expect{|
val functional_update : string = "updated"
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
  let r : t = Marshal.from_string (Marshal.to_string { x = #() } []) 0 in
  let #() = r.x in
  "marshaled"
[%%expect{|
val marshal_round_trip : string = "marshaled"
|}]

(* All-void records are ordinary values when stored in other blocks. *)

let nested_length = List.length [{ x = #() }; { x = #() }; { x = #() }]
[%%expect{|
val nested_length : int = 3
|}]

(* Recursive bindings. *)

let letrec_match =
  let rec r = { x = #() }
  and f () = r in
  let #() = (f ()).x in
  "matched"
[%%expect{|
val letrec_match : string = "matched"
|}]

(* Kinds: an all-void record is a pointer, not an immediate. *)

type bad : immediate = { x : unit# }
[%%expect{|
Line 1, characters 0-36:
1 | type bad : immediate = { x : unit# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "bad" is value non_float
         because it's a boxed record type.
       But the layout of type "bad" must be a sublayout of value non_pointer
         because of the annotation on the declaration of the type bad.
       Note: The layout of immediate is value non_pointer.
       Note: The kinds mutable_data, immutable_data, and sync_data have
       the layout value non_float.
|}]
