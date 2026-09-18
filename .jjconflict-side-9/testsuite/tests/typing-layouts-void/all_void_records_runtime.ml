(* TEST
 flambda2;
 include stdlib_stable;
 flags = "-extension layouts_beta";
 { expect; expect.opt; }
 { flags += " -Oclassic"; expect.opt; }
 { flags += " -O3"; expect.opt; }
*)

(* [void] expressions are evaluated exactly once for their side effects. *)
type t = { x : unit#; kept : unit# }
type m = { mutable z : unit# }
[%%expect{|
type t = { x : unit#; kept : unit#; }
type m = { mutable z : unit#; }
|}]

(* Measure a side effect that increments a counter then optionally raises. *)
let () =
  let check name (f : (unit -> unit) -> unit) =
    let run ~should_raise =
      let calls = ref 0 in
      let eff () = incr calls; if should_raise then raise Exit in
      let outcome = try let () = f eff in "returned" with Exit -> "raised" in
      !calls, outcome
    in
    let calls, outcome = run ~should_raise:false in
    let raised_calls, raised_outcome = run ~should_raise:true in
    Format.printf "%18s: %s with %d; %s with %d@."
      name outcome calls raised_outcome raised_calls
  in
  let r = { x = #(); kept = #() } in
  let m = { z = #() } in
  check "nothing" (fun eff -> ());
  check "twice" (fun eff ->
    let _ = (eff (), eff ()) in ());
  check "five" (fun eff ->
    let _ = (eff (), eff (), eff (), eff (), eff ()) in ());
  check "construct" (fun eff ->
    let _ : t = { x = (eff (); #()); kept = #() } in ());
  check "project" (fun eff ->
    let #() = (eff (); r).x in ());
  check "set receiver" (fun eff -> (eff (); m).z <- #());
  check "set value" (fun eff -> m.z <- (eff (); #()));
  check "update receiver" (fun eff ->
    let _ : t = { (eff (); r) with x = #() } in ());
  check "update value" (fun eff ->
    let _ : t = { r with x = (eff (); #()) } in ());
  check "index get receiver" (fun eff ->
    let #() = Stdlib_stable.Idx_mut.get (eff (); m) (.z) in ());
  check "index get index" (fun eff ->
    let #() = Stdlib_stable.Idx_mut.get m (eff (); (.z)) in ());
  check "index set receiver" (fun eff ->
    Stdlib_stable.Idx_mut.set (eff (); m) (.z) #());
  check "index set index" (fun eff ->
    Stdlib_stable.Idx_mut.set m (eff (); (.z)) #());
  check "index set value" (fun eff ->
    Stdlib_stable.Idx_mut.set m (.z) (eff (); #()))
[%%expect{|
           nothing: returned with 0; returned with 0
             twice: returned with 2; raised with 1
              five: returned with 5; raised with 1
         construct: returned with 1; raised with 1
           project: returned with 1; raised with 1
      set receiver: returned with 1; raised with 1
         set value: returned with 1; raised with 1
   update receiver: returned with 1; raised with 1
      update value: returned with 1; raised with 1
index get receiver: returned with 1; raised with 1
   index get index: returned with 1; raised with 1
index set receiver: returned with 1; raised with 1
   index set index: returned with 1; raised with 1
   index set value: returned with 1; raised with 1
|}]

(* Same behavior as above when specializing a generic type parameter. *)
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

(* Marshaling, comparison, and hashing are structural, not pointer-wise. *)
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
