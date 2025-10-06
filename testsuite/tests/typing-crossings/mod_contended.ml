(* NOTES:
    - maybe this file should move, but for now I'm just throwing examples in here
    - initially, this is just for mod contended and none of the other mod stuff,
      nor mod shared (which might exist)
*)

(* Q: should this be inferring as x @ ... mod contended?
      since int crosses everything, does this get legacy mode currently? *)
(* I think that this question gets more interesting if we track both modes and mods.
   This is certainly a bad test to keep around but good to think about, for me at least *)
let x = 1 in x

(* x should be inferred to be mod contended *)
let mt : int ref list = [] in
  let f @ portable = fun () -> (mt : _ @ uncontended) in
  ()

(* this should NOT typecheck, since in general x does not cross contention *)
let foo (x : int ref option) =
  let sg = [ x ] in
  let f @ portable = fun () -> (sg : _ @ uncontended) in
  ()

(* extension: support annotations in negative positions *)
let foo (x : int ref option mod contended) =
  let sg = [ x ] in
  let f @ portable = fun () -> (sg : _ @ uncontended) in
  ()

type boxed_rec = { a : int ref option; b : int }
type boxed_rec_mut = { mutable a : int ref option; b : int }

type variant =
  | Has_rec of int * bool * r
  | Has_rec_mut of int * bool * rmut

let x = Has_rec (1, true, { a = None; b = 4 }) in
  let f @ portable = fun () -> (x : _ @ uncontended) in
  ()

(* this should NOT typecheck, since the record has a mutable field *)
let x = Has_rec_mut (1, true, { a = None; b = 4 }) in
  let f @ portable = fun () -> (x : _ @ uncontended) in
  ()

let x : int array = [| |] in
  let f @ portable = fun () -> (x : _ @ uncontended) in
  ()

(* this should NOT typecheck, since arrays are mutable *)
let x : int array = [| 1 |] in
  let f @ portable = fun () -> (x : _ @ uncontended) in
  ()

let x : int ref iarray = [: :] in
  let f @ portable = fun () -> (x : _ @ uncontended) in
  ()

(* this should NOT typecheck, since the contents of the iarray are mutable *)
let x : int ref iarray = [: ref 1 :] in
  let f @ portable = fun () -> (x : _ @ uncontended) in
  ()

(* this seems like more of an extension; in the impl it looks like this might not
   "just work". maybe this will "just work" but p actually being mod contended won't *)
(* Q: what was the deal with tuple_modes? I remembered some work but others don't *)
let ((x, y) as p) : int ref list * int ref list = ([], [ ref 1 ]) in
  let f @ portable = fun () -> (x : _ @ uncontended) in
  (* these two cases should NOT work, however... *)
  let g @ portable = fun () -> (y : _ @ uncontended) in
  let h @ portable = fun () -> (p : _ @ uncontended) in
  ()

(* I don't think that unboxed records should change how this works, but it is still
   worth trying out some examples with nested unboxed records combining with mutable *)
(* does mixed blocks make a difference at all? *)

(* this should just not work, even without mod contended, right? since let mutable
   cannot be captured, even if the closure doesn't escape, IIUC

let mutable x : int ref list = [] in
  let f @ portable = fun () -> (x : _ @ uncontended) in
  () *)

(* calling a function doesn't seem like something that we initially will be able to
   support, but if the function is annotated as returning mod contended that seems
   doable (again, as an extension) *)

(* there are surely some more built-in cases, like polymorphic variants, GADTs,
   maybe even comprehensions (I don't know ho these work). oh and lazy too.?
   as a far-out extension, possibly first-class modules too but that doesn't
   sound very useful to me. worth noting since I thought of it, though... *)
