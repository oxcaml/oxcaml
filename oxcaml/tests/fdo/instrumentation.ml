(* The counters [Fdo_instrumentation] attaches during the translation from
   Lambda. *)

external c_call : int -> int = "caml_fdo_test_c_call"

let[@inline never] f x = x + 1

(* An [if] is a switch with an edge per arm. C calls get no counter. *)
let[@inline never] branches x = if x > 0 then f x else c_call x

(* Switches introduced by the translation, like the bounds check of an array
   access with its cold failure arm, get no counters. *)
let[@inline never] bounds_checked a i = a.(i)

(* A [match] can be several switches, numbered in order with the calls. Each
   [Lambda] switch has one index, shared by the switches it translates to. *)
let[@inline never] matches = function
  | None -> f 0
  | Some 0 -> 1
  | Some n -> f n

type t =
  | C0
  | C1
  | C2
  | B0 of int
  | B1 of int
  | B2 of int

(* A match on both constant constructors and blocks is one [Lambda] switch,
   whose arms are numbered by constant, then by tag. It translates to a switch
   on the constants and one on the tags, behind a dispatch on whether the
   scrutinee is an immediate, which gets no counters. Constructors that share an
   arm keep a counter each. *)
let[@inline never] variants = function
  | C0 -> f 0
  | C1 | C2 -> 1
  | B0 n -> f n
  | B1 n | B2 n -> n

(* Constructors without an arm of their own go to the default arm, numbered
   after the constants and tags, and get its counter. *)
let[@inline never] defaults = function C0 -> f 0 | B0 n -> f n | _ -> 2

let[@inline never] apply k x = k x

(* Local and anonymous functions number their own bodies. *)
let[@inline never] nested n =
  let g y = if y > n then f y else y in
  apply (fun z -> g (g z)) n

let[@inline never] add x y = x + y

(* The counter of a partial application goes on the call in the wrapper that
   closure conversion builds for it, and an over-application has a single
   counter, on the call to the callee. *)
let[@inline never] partial x = add x

let[@inline never] over x = apply add x 1

(* In the unit's toplevel code: a call bound to a value is an interior call of
   the toplevel code, while calls with no binding of their own are numbered
   within the unit's anonymous module. *)
let v = f 0

let () = ignore (f 1)

let _ = f 2;;

ignore (f 3);;

module type S = sig
  val x : int
end

(* A functor body is a function: a call bound to a value is interior, and other
   calls are module-level calls of the functor's binding. *)
module F (X : S) = struct
  let y = f X.x

  let () = ignore (f y)
end

module A = struct
  let x = v
end

(* Functor applications are module-level calls, numbered within their binding,
   or within the unit's anonymous module if they have none. *)
module M = F (A)

module N = F (struct
  include F (A)

  let x = y
end)

module _ = F (A)
include F (A)

(* The module-level calls of a function's local modules are named by the
   function's path. *)
let[@inline never] local () =
  let module L = F (A) in
  let module _ = F (A) in
  L.y
