(* TEST
 modules = "replace_caml_atomic.c";
 {
   not-macos;
   arch_amd64;
   flags = "-cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_atomic_load \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_atomic_load_field \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_atomic_exchange \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_atomic_exchange_field \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_atomic_set \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_atomic_set_field \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_atomic_compare_exchange \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_atomic_compare_exchange_field \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_atomic_cas \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_atomic_cas_field \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_atomic_fetch_add \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_atomic_fetch_add_field \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_atomic_add \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_atomic_add_field \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_atomic_sub \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_atomic_sub_field \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_atomic_land \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_atomic_land_field \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_atomic_lor \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_atomic_lor_field \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_atomic_lxor \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_atomic_lxor_field";
   native;
 }
*)

(* CR-someday mslater: this should also work on arm once atomics are builtins *)

(* This test verifies that immediate atomics do not call runtime wrapper functions
   in native amd64 builds. *)

external total_atomic_calls : unit -> int = "total_atomic_calls"
external total_atomic_reset : unit -> unit = "total_atomic_reset"

external atomic_load_calls : unit -> int = "atomic_load_calls"
external atomic_load_field_calls : unit -> int = "atomic_load_field_calls"
external atomic_exchange_calls : unit -> int = "atomic_exchange_calls"
external atomic_exchange_field_calls : unit -> int = "atomic_exchange_field_calls"
external atomic_set_calls : unit -> int = "atomic_set_calls"
external atomic_set_field_calls : unit -> int = "atomic_set_field_calls"
external atomic_compare_exchange_calls : unit -> int = "atomic_compare_exchange_calls"
external atomic_compare_exchange_field_calls : unit -> int = "atomic_compare_exchange_field_calls"
external atomic_cas_calls : unit -> int = "atomic_cas_calls"
external atomic_cas_field_calls : unit -> int = "atomic_cas_field_calls"
external atomic_fetch_add_calls : unit -> int = "atomic_fetch_add_calls"
external atomic_fetch_add_field_calls : unit -> int = "atomic_fetch_add_field_calls"
external atomic_add_calls : unit -> int = "atomic_add_calls"
external atomic_add_field_calls : unit -> int = "atomic_add_field_calls"
external atomic_sub_calls : unit -> int = "atomic_sub_calls"
external atomic_sub_field_calls : unit -> int = "atomic_sub_field_calls"
external atomic_land_calls : unit -> int = "atomic_land_calls"
external atomic_land_field_calls : unit -> int = "atomic_land_field_calls"
external atomic_lor_calls : unit -> int = "atomic_lor_calls"
external atomic_lor_field_calls : unit -> int = "atomic_lor_field_calls"
external atomic_lxor_calls : unit -> int = "atomic_lxor_calls"
external atomic_lxor_field_calls : unit -> int = "atomic_lxor_field_calls"

external atomic_load_reset : unit -> unit = "atomic_load_reset"
external atomic_load_field_reset : unit -> unit = "atomic_load_field_reset"
external atomic_exchange_reset : unit -> unit = "atomic_exchange_reset"
external atomic_exchange_field_reset : unit -> unit = "atomic_exchange_field_reset"
external atomic_set_reset : unit -> unit = "atomic_set_reset"
external atomic_set_field_reset : unit -> unit = "atomic_set_field_reset"
external atomic_compare_exchange_reset : unit -> unit = "atomic_compare_exchange_reset"
external atomic_compare_exchange_field_reset : unit -> unit = "atomic_compare_exchange_field_reset"
external atomic_cas_reset : unit -> unit = "atomic_cas_reset"
external atomic_cas_field_reset : unit -> unit = "atomic_cas_field_reset"
external atomic_fetch_add_reset : unit -> unit = "atomic_fetch_add_reset"
external atomic_fetch_add_field_reset : unit -> unit = "atomic_fetch_add_field_reset"
external atomic_add_reset : unit -> unit = "atomic_add_reset"
external atomic_add_field_reset : unit -> unit = "atomic_add_field_reset"
external atomic_sub_reset : unit -> unit = "atomic_sub_reset"
external atomic_sub_field_reset : unit -> unit = "atomic_sub_field_reset"
external atomic_land_reset : unit -> unit = "atomic_land_reset"
external atomic_land_field_reset : unit -> unit = "atomic_land_field_reset"
external atomic_lor_reset : unit -> unit = "atomic_lor_reset"
external atomic_lor_field_reset : unit -> unit = "atomic_lor_field_reset"
external atomic_lxor_reset : unit -> unit = "atomic_lxor_reset"
external atomic_lxor_field_reset : unit -> unit = "atomic_lxor_field_reset"

(* Reset all atomic counters. Initializing stdlib modules (like Format) can
   invoke atomic operations. *)

let () =
  total_atomic_reset ();
  atomic_load_reset ();
  atomic_load_field_reset ();
  atomic_exchange_reset ();
  atomic_exchange_field_reset ();
  atomic_set_reset ();
  atomic_set_field_reset ();
  atomic_compare_exchange_reset ();
  atomic_compare_exchange_field_reset ();
  atomic_cas_reset ();
  atomic_cas_field_reset ();
  atomic_fetch_add_reset ();
  atomic_fetch_add_field_reset ();
  atomic_add_reset ();
  atomic_add_field_reset ();
  atomic_sub_reset ();
  atomic_sub_field_reset ();
  atomic_land_reset ();
  atomic_land_field_reset ();
  atomic_lor_reset ();
  atomic_lor_field_reset ();
  atomic_lxor_reset ();
  atomic_lxor_field_reset ()

let a = Atomic.make 0
let _ = Atomic.get a
let _ = Atomic.set a 1
let _ = Atomic.exchange a 2
let _ = Atomic.compare_and_set a 2 3
let _ = Atomic.compare_exchange a 3 4
let _ = Atomic.fetch_and_add a 1
let _ = Atomic.add a 1
let _ = Atomic.sub a 1
let _ = Atomic.logand a 1
let _ = Atomic.logor a 1
let _ = Atomic.logxor a 1

type 'a atomic = { mutable x : 'a [@atomic] }

let a = {x = 0}
let _ = a.x
let _ = a.x <- 1

let a = {x = 0}
let _ = Atomic.Loc.get [%atomic.loc a.x]
let _ = Atomic.Loc.set [%atomic.loc a.x] 1
let _ = Atomic.Loc.exchange [%atomic.loc a.x] 2
let _ = Atomic.Loc.compare_and_set [%atomic.loc a.x] 2 3
let _ = Atomic.Loc.compare_exchange [%atomic.loc a.x] 3 4
let _ = Atomic.Loc.fetch_and_add [%atomic.loc a.x] 1
let _ = Atomic.Loc.add [%atomic.loc a.x] 1
let _ = Atomic.Loc.sub [%atomic.loc a.x] 1
let _ = Atomic.Loc.logand [%atomic.loc a.x] 1
let _ = Atomic.Loc.logor [%atomic.loc a.x] 1
let _ = Atomic.Loc.logxor [%atomic.loc a.x] 1

external atomic_get_field : 'a atomic -> int -> 'a = "%atomic_load_field"
external atomic_set_field : 'a atomic -> int -> 'a -> unit = "%atomic_set_field"
external atomic_exchange_field : 'a atomic -> int -> 'a -> 'a = "%atomic_exchange_field"
external atomic_compare_exchange_field : 'a atomic -> int -> 'a -> 'a -> 'a = "%atomic_compare_exchange_field"
external atomic_compare_and_set_field : 'a atomic -> int -> 'a -> 'a -> bool = "%atomic_cas_field"
external atomic_fetch_and_add_field : int atomic -> int -> int -> int = "%atomic_fetch_add_field"
external atomic_add_field : int atomic -> int -> int -> unit = "%atomic_add_field"
external atomic_sub_field : int atomic -> int -> int -> unit = "%atomic_sub_field"
external atomic_logand_field : int atomic -> int -> int -> unit = "%atomic_land_field"
external atomic_logor_field : int atomic -> int -> int -> unit = "%atomic_lor_field"
external atomic_logxor_field : int atomic -> int -> int -> unit = "%atomic_lxor_field"

let a = {x = 0}
let _ = atomic_get_field a 0
let _ = atomic_set_field a 0 1
let _ = atomic_exchange_field a 0 2
let _ = atomic_compare_and_set_field a 0 2 3
let _ = atomic_compare_exchange_field a 0 3 4
let _ = atomic_fetch_and_add_field a 0 1
let _ = atomic_add_field a 0 1
let _ = atomic_sub_field a 0 1
let _ = atomic_logand_field a 0 1
let _ = atomic_logor_field a 0 1
let _ = atomic_logxor_field a 0 1

let () = assert (atomic_load_calls () = 0)
let () = assert (atomic_load_field_calls () = 0)
let () = assert (atomic_exchange_calls () = 0)
let () = assert (atomic_exchange_field_calls () = 0)
let () = assert (atomic_set_calls () = 0)
let () = assert (atomic_set_field_calls () = 0)
let () = assert (atomic_compare_exchange_calls () = 0)
let () = assert (atomic_compare_exchange_field_calls () = 0)
let () = assert (atomic_cas_calls () = 0)
let () = assert (atomic_cas_field_calls () = 0)
let () = assert (atomic_fetch_add_calls () = 0)
let () = assert (atomic_fetch_add_field_calls () = 0)
let () = assert (atomic_add_calls () = 0)
let () = assert (atomic_add_field_calls () = 0)
let () = assert (atomic_sub_calls () = 0)
let () = assert (atomic_sub_field_calls () = 0)
let () = assert (atomic_land_calls () = 0)
let () = assert (atomic_land_field_calls () = 0)
let () = assert (atomic_lor_calls () = 0)
let () = assert (atomic_lor_field_calls () = 0)
let () = assert (atomic_lxor_calls () = 0)
let () = assert (atomic_lxor_field_calls () = 0)

(* Test individual atomic operations. *)

(* build a test function for a particular atomic call *)
let gen_test ~fn ~fn_calls ~reset_fn_calls =
  let test ~(call_pos : [%call_pos]) ~expected f =
    total_atomic_reset ();
    reset_fn_calls ();
    f ();
    let actual_fn = fn_calls () in
    let actual_total = total_atomic_calls () in
    if not (expected = actual_fn) then
      failwith @@
        Format.sprintf
          "On line %d, expected %d calls to %s, but saw %d"
          call_pos.pos_lnum expected fn actual_fn;
    if not (expected = actual_total) then
      failwith @@
        Format.sprintf
          "On line %d, expected %d total atomic calls, but saw %d"
          call_pos.pos_lnum expected actual_total;
  in
  test

let test_atomic_exchange_field = gen_test ~fn:"atomic_exchange_field"
                              ~fn_calls:atomic_exchange_field_calls
                              ~reset_fn_calls:atomic_exchange_field_reset

(* Patomic_set_field skips runtime call for immediates. *)
type t = { mutable imm: int [@atomic]; mutable ptr: string [@atomic] }

let () =
  let t = { imm = 1; ptr = "two"} in
  test_atomic_exchange_field ~expected:1 (fun () ->
    t.imm <- 3;
    t.ptr <- "four";
    ignore (Sys.opaque_identity t)
  )

(* Patomic_set_mixed_field skips runtime call for immediates. *)
type mixed = { f : int64#; mutable imm: int [@atomic]; mutable ptr: string [@atomic] }
let () =
  let m = { f = #42L; imm = 1; ptr = "two"} in
  test_atomic_exchange_field ~expected:1 (fun () ->
    m.imm <- 3;
    m.ptr <- "four";
    ignore (Sys.opaque_identity m)
  )
