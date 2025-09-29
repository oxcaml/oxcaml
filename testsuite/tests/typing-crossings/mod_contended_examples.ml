(* NOTES: these examples should typecheck in the very near future *)

type boxed_rec = { a : int ref option; b : int }
type boxed_rec_mut = { mutable a : int ref option; b : int }

type variant =
  | Has_rec of int * bool * boxed_rec
  | Has_rec_mut of int * bool * boxed_rec_mut

let x = Has_rec (1, true, { a = None; b = 4 }) in
  let f @ portable = fun () -> (x : _ @ uncontended) in
  ()

(* this should NOT typecheck, since the record has a mutable field *)
let x = Has_rec_mut (1, true, { a = None; b = 4 }) in
  let f @ portable = fun () -> (x : _ @ uncontended) in
  ()

(* extension: support annotations in negative positions *)
let foo (x : int ref option mod contended) =
  let sg = [ x ] in
  let f @ portable = fun () -> (sg : _ @ uncontended) in
  ()
