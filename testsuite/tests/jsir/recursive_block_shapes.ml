(* TEST
 flambda2;
 flags = "-extension layouts_beta";
 { bytecode; }
 { native; }
 { jsir; }
*)

(* Recursive dummies must have the same shape as the blocks that replace them:
   JSIR flattens nested products and erases void fields, unlike bytecode. *)

type values = { payload : #(int * #(string * int)); next : values }

let[@inline never] make_values n =
  let rec x = { payload = #(n, #("a", n + 1)); next = x } in
  x

let () =
  let x = make_values 1 in
  let #(n, #(s, m)) = x.payload in
  assert (n = 1 && s = "a" && m = 2);
  assert (x.next == x);
  let copy = { payload = x.payload; next = x } in
  assert (Obj.size (Obj.repr x) = Obj.size (Obj.repr copy))

type mixed = { payload : #(int * #(unit# * float#)); next : mixed }

external box_float : float# -> float = "%box_float"

let[@inline never] make_mixed n =
  let rec x = { payload = #(n, #(#(), #2.5)); next = x } in
  x

let () =
  let x = make_mixed 3 in
  let #(n, #(#(), f)) = x.payload in
  assert (n = 3 && box_float f = 2.5);
  assert (x.next == x);
  let copy = { payload = x.payload; next = x } in
  assert (Obj.size (Obj.repr x) = Obj.size (Obj.repr copy))

type with_void = { erased : unit#; n : int; next : with_void }

let[@inline never] make_with_void n =
  let rec x = { erased = #(); n; next = x } in
  x

let () =
  let x = make_with_void 4 in
  assert (x.n = 4 && x.next == x);
  let copy = { erased = #(); n = x.n; next = x } in
  assert (Obj.size (Obj.repr x) = Obj.size (Obj.repr copy))

let[@inline never] make_recursive_functions n =
  let rec f (_ : unit#) =
    if !n = 0 then 0 else (decr n; g #())
  and g (_ : unit#) = 1 + f #() in
  f, g

let () =
  let n = ref (Sys.opaque_identity 3) in
  let f, g = Sys.opaque_identity (make_recursive_functions n) in
  assert (f #() = 3);
  assert (g #() = 1)
