(* The payload types are our own: [elt] immutable, [cell] mutable, so any crossing in the
   outputs is declared in the mli, not incidental. *)
type elt = { fixed : unit }
type arrow = elt -> elt

type flag =
  | On
  | Off

type cell = { mutable contents : elt }

(* A polymorphic mutable box: instantiating [_ box] with an inferred arrow keeps mode
   variables in the stored function's type, unlike a monomorphic field. *)
type 'a box = { mutable v : 'a }

let return_first (a : elt) (_ : elt) = a
let apply f x = f x
let id x = x

let choose f x y =
  match f with
  | On -> x
  | Off -> y
;;

let unstaged (_ : elt) (_ : elt) (_ : elt) = ()

(* Returns a 1-ary closure. *)
let staged_1ary (_ : elt) =
  let g (_ : elt) = () in
  g
;;

(* Returns a 2-ary closure. Arity comes from the type's spelled spine, so the staging
   does not cap the closure's params. *)
let staged_2ary (_ : elt) =
  let g (_ : elt) (_ : elt) = () in
  g
;;

(* This is treated as a 3-ary function by the strengthener. *)
let staged_via_fun_syntax = fun (_ : elt) -> fun (_ : elt) -> fun (_ : elt) -> ()

(* Point-free alias of [unstaged]: arity comes from the type, not the binding's syntax,
   so the suggestions match [unstaged]'s. *)
let point_free = unstaged

let call_twice f x = f (f x)
let store c x = c.contents <- x

(* Legacy toplevel mutable capturing a function. *)
let cache = { v = (fun (x : elt) -> x) }
let retain f = cache.v <- f

let use (f : arrow) x = f x

let apply_once (type b) f x =
  let _ : b = f x in
  ()
;;

let apply_twice (type b) f x =
  let _ : b = f x in
  let _ : b = f x in
  ()
;;

let churn_local f =
  let _ = f (f { contents = { fixed = () } }) in
  ()
;;

let churn_nonlocal f =
  let _ = f (f { contents = { fixed = () } }) in
  ()
;;

(* Four levels of argument nesting: each level's arrows get their own edits. *)
let nest3 f = f (fun g -> g (fun (x : elt) -> x))

(* [f]'s result flows into [g], so their modes are linked through [r]; [relay_pinned]'s
   mli annotates [g]'s parameter. *)
let relay f g =
  let r = { contents = { fixed = () } } in
  g (f r)
;;

let relay_pinned f g =
  let r = { contents = { fixed = () } } in
  g (f r)
;;

(* The same function [f] reaches depth 4 of both arguments (monomorphic by usage, so its
   arrow's mode variables are shared, not legacy-pinned); [weave_pinned]'s mli annotates
   depth 4 inside [u]'s type only. *)
let weave u v =
  let f c = { contents = c.contents } in
  u (fun g -> g f);
  v (fun h -> h f)
;;

let weave_pinned u v =
  let f c = { contents = c.contents } in
  u (fun g -> g f);
  v (fun h -> h f)
;;

(* [app_pinned]'s mli marks [f]'s parameter [@ local], which frees [x], a different
   top-level argument, to be borrowed. *)
let app f x = f x
let app_pinned f x = f x
