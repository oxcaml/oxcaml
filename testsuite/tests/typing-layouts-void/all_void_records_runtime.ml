(* TEST
 flambda2;
 include stdlib_stable;
 flags = "-extension layouts_beta";
 { expect; expect.opt; }
 { flags += " -Oclassic"; expect.opt; }
 { flags += " -O3"; expect.opt; }
*)

(* Void operands are still evaluated exactly once, including when they
   raise. Only one operand has effects in each row, so no evaluation order
   is prescribed. The partial updates also read a retained void field. *)
type t = { x : unit#; kept : unit# }
type m = { mutable z : unit# }
[%%expect{|
type t = { x : unit#; kept : unit#; }
type m = { mutable z : unit#; }
|}]

let () =
  let check name f =
    let run should_raise =
      let calls = ref 0 in
      let tick () = incr calls; if should_raise then raise Exit in
      let outcome = try let () = f tick in "returned" with Exit -> "raised" in
      !calls, outcome
    in
    let calls, outcome = run false in
    let raised_calls, raised_outcome = run true in
    Format.printf "%s: %d %s; %d %s@."
      name calls outcome raised_calls raised_outcome
  in
  let r = { x = #(); kept = #() } in
  let m = { z = #() } in
  check "construct" (fun tick ->
    let _ : t = { x = (tick (); #()); kept = #() } in ());
  check "project" (fun tick ->
    let #() = (tick (); r).x in ());
  check "set receiver" (fun tick -> (tick (); m).z <- #());
  check "set value" (fun tick -> m.z <- (tick (); #()));
  check "update receiver" (fun tick ->
    let _ : t = { (tick (); r) with x = #() } in ());
  check "update value" (fun tick ->
    let _ : t = { r with x = (tick (); #()) } in ());
  check "index get receiver" (fun tick ->
    let #() = Stdlib_stable.Idx_mut.get (tick (); m) (.z) in ());
  check "index get index" (fun tick ->
    let #() = Stdlib_stable.Idx_mut.get m (tick (); (.z)) in ());
  check "index set receiver" (fun tick ->
    Stdlib_stable.Idx_mut.set (tick (); m) (.z) #());
  check "index set index" (fun tick ->
    Stdlib_stable.Idx_mut.set m (tick (); (.z)) #());
  check "index set value" (fun tick ->
    Stdlib_stable.Idx_mut.set m (.z) (tick (); #()))
[%%expect{|
construct: 1 returned; 1 raised
project: 1 returned; 1 raised
set receiver: 1 returned; 1 raised
set value: 1 returned; 1 raised
update receiver: 1 returned; 1 raised
update value: 1 returned; 1 raised
index get receiver: 1 returned; 1 raised
index get index: 1 returned; 1 raised
index set receiver: 1 returned; 1 raised
index set index: 1 returned; 1 raised
index set value: 1 returned; 1 raised
|}]

(* Specializing a generic record to a void product preserves construction,
   mutation, and matching, including evaluation of every product component. *)
type ('a : any) generic = { mutable field : 'a }
let generic_round_trip =
  let log = ref [] in
  let mark name = log := name :: !log; #() in
  let r : #(unit# * unit#) generic =
    { field = #(mark "first", mark "second") }
  in
  r.field <- #(mark "third", mark "fourth");
  let { field = #(a, b) } = r in
  let #() = a in
  let #() = b in
  List.sort String.compare !log
[%%expect{|
type ('a : any) generic = { mutable field : 'a; }
val generic_round_trip : String.t list =
  ["first"; "fourth"; "second"; "third"]
|}]

(* A functor can box and project a field whose type is abstract but void. *)
let abstract_void =
  let module Void : sig type t : void val make : unit -> t end = struct
    type t = unit#
    let make () = #()
  end in
  let module Box (V : sig type t : void end) = struct
    type t = { field : V.t }
    let make f = { field = f () }
    let project r = let (_ : V.t) = r.field in "projected"
  end in
  let module B = Box (Void) in
  B.project (B.make Void.make)
[%%expect{|
val abstract_void : string = "projected"
|}]

(* Marshaling, structural comparison, and hashing depend on contents, not
   physical identity. *)
let round_trip =
  let original = { x = #(); kept = #() } in
  let restored : t = Marshal.from_string (Marshal.to_string original []) 0 in
  let #() = restored.x in
  original = restored,
  compare original restored,
  Hashtbl.hash original = Hashtbl.hash restored
[%%expect{|
val round_trip : bool * int * bool = (true, 0, true)
|}]
