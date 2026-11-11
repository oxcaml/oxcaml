(* TEST
 modules = "cstubs.c";
 flags = "-runtime-variant=d";
 stack-allocation;
 native;
*)

(* A fiber can be given a pointer into the local stack of one of its ancestors,
   as long as the pointer is hidden from the type checker and the GC: here it
   is passed as an unboxed integer. The ancestors of a running fiber are
   suspended beneath it, so the pointee stays allocated and is scanned by the
   GC along with the rest of the fiber chain. The runtime must hence treat it
   like any other local allocation. This test checks that at each place where
   the runtime asks whether a value is local, under the debug runtime's
   assertions. *)

module Stack_pointer = struct
  type 'a t = nativeint_u

  external of_value : 'a @ local -> 'a t
    = "" "caml_native_pointer_of_value"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external to_value : 'a t -> 'a @ local
    = "" "caml_native_pointer_to_value"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]
end

external opaque : ('a[@local_opt]) -> ('a[@local_opt]) = "%opaque"

(* Run [f] in a fiber [depth] levels below the current one. *)
let rec in_fiber ~depth f =
  if depth = 0
  then f ()
  else
    Effect.Deep.try_with
      (fun () -> in_fiber ~depth:(depth - 1) f)
      ()
      { effc = (fun (type a) (_ : a Effect.t) -> None) }

(* Apply [f] to [v] in a fiber [depth] levels below the current one. *)
let[@inline never] from_fiber (v : 'a @ local) ~depth (f : 'a @ local -> unit) =
  let ptr = Stack_pointer.of_value v in
  in_fiber ~depth (fun () -> f (Stack_pointer.to_value ptr) [@nontail]);
  (* The fiber's reference to [v] is hidden from the GC, so keep [v] alive. *)
  let _ : 'a = opaque v in
  ()

type 'a cell =
  { mutable plain : 'a;
    mutable atomic : 'a [@atomic]
  }

let[@inline never] fresh i = ref i

(* [caml_modify_local] *)
let test_modify ~depth =
  let cell = stack_ { plain = fresh 0; atomic = fresh 0 } in
  from_fiber cell ~depth (fun cell ->
    cell.plain <- fresh 1;
    Gc.full_major ();
    assert (!(cell.plain) = 1));
  assert (!(cell.plain) = 1)

(* [caml_atomic_exchange_field_local], [caml_atomic_cas_field_local] and
   [caml_atomic_compare_exchange_field_local] *)
let test_atomic ~depth =
  let cell = stack_ { plain = fresh 0; atomic = fresh 0 } in
  from_fiber cell ~depth (fun cell ->
    let v1 = fresh 1 and v2 = fresh 2 and v3 = fresh 3 in
    let v0 = Atomic.Loc.exchange [%atomic.loc cell.atomic] v1 in
    let swapped = Atomic.Loc.compare_and_set [%atomic.loc cell.atomic] v1 v2 in
    let seen = Atomic.Loc.compare_exchange [%atomic.loc cell.atomic] v2 v3 in
    assert (!v0 = 0 && swapped && seen == v2);
    Gc.full_major ();
    assert (!(cell.atomic) = 3));
  assert (!(cell.atomic) = 3)

external blit
  : 'a array @ local -> int -> 'a array @ local -> int -> int -> unit
  = "caml_array_blit"

external fill : 'a array @ local -> int -> int -> 'a -> unit = "caml_array_fill"

external remembered_set_size : unit -> int = "remembered_set_size" [@@noalloc]

(* [caml_uniform_array_blit] and [caml_uniform_array_fill]. Every GC scans the
   whole of a local array, so writes to one must not use the remembered set:
   its entries could outlive the array. *)
let test_array ~depth =
  let dst = stack_ [| fresh 0; fresh 0 |] in
  from_fiber dst ~depth (fun dst ->
    (* Promote the contents of [dst], so that overwriting them with young
       values would need the remembered set if [dst] were on the major heap. *)
    Gc.minor ();
    let src = [| fresh 1; fresh 1 |] in
    let size = remembered_set_size () in
    blit src 0 dst 0 2;
    assert (remembered_set_size () = size);
    Gc.full_major ();
    assert (!(dst.(0)) = 1 && !(dst.(1)) = 1);
    let v = fresh 2 in
    let size = remembered_set_size () in
    fill dst 0 2 v;
    assert (remembered_set_size () = size);
    Gc.full_major ();
    assert (!(dst.(0)) = 2 && !(dst.(1)) = 2));
  assert (!(dst.(0)) = 2 && !(dst.(1)) = 2)

external is_stack : 'a @ local -> bool = "caml_obj_is_stack"

(* [caml_obj_is_stack] *)
let test_is_stack ~depth =
  let cell = stack_ { plain = fresh 0; atomic = fresh 0 } in
  from_fiber cell ~depth (fun cell -> assert (is_stack cell)) [@nontail]

external callback2
  : ('a @ local -> unit -> 'a @ local) -> 'a @ local -> unit -> 'a @ local
  = "test_callback2"

external callback3
  :  ('a @ local -> unit -> unit -> 'a @ local)
  -> 'a @ local -> unit -> unit -> 'a @ local
  = "test_callback3"

external callbackN
  :  ('a @ local -> unit -> unit -> unit -> 'a @ local)
  -> 'a @ local -> unit -> unit -> unit -> 'a @ local
  = "test_callbackN"

(* [caml_callback2], [caml_callback3] and [caml_callbackN] reset the local
   stack of the fiber they run on, so their result must not be on it. A result
   on the local stack of an ancestor is unaffected. *)
let test_callback ~depth =
  let cell = stack_ { plain = fresh 0; atomic = fresh 0 } in
  from_fiber cell ~depth (fun cell ->
    assert (callback2 (fun x () -> x) cell () == cell);
    assert (callback3 (fun x () () -> x) cell () () == cell);
    assert (callbackN (fun x () () () -> x) cell () () () == cell))
  [@nontail]

let tests ~depth =
  test_modify ~depth;
  test_atomic ~depth;
  test_array ~depth;
  test_is_stack ~depth;
  test_callback ~depth

(* Access the local stack of a parent and of a grandparent. *)
let run () =
  tests ~depth:1;
  tests ~depth:2

(* Allocate on the local stack of the main fiber, then on that of a child. *)
let () =
  run ();
  in_fiber ~depth:1 run
