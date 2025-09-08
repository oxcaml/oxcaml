# 2 "thread.ml"

(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt     *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* User-level threads *)

[@@@ocaml.flambda_o3]

(* Implementation copied from [Domain.DLS], modulo thread-safety concerns. *)
module TLS = struct
  module Obj_opt : sig @@ portable
    type t
    val none : t
    val some : 'a -> t
    val is_some : t -> bool
    val unsafe_get : t -> 'a
  end = struct
    type t = Obj.t
    let none = Obj.magic_portable (Obj.repr (ref 0))
    let some v = Obj.repr v
    let is_some obj = (obj != Obj.magic_uncontended none)
    let unsafe_get obj = Obj.obj obj
  end

  type tls_state = Obj_opt.t array

  external get_tls_state : unit -> tls_state @@ portable = "caml_thread_get_state"
  external set_tls_state : tls_state -> unit @@ portable = "caml_thread_set_state"

  let init () =
    let state = Array.make 8 (Obj.magic_uncontended Obj_opt.none) in
    set_tls_state state

  type 'a key = int * (unit -> 'a) Modes.Portable.t

  let key_counter = Atomic.make 0

  type key_initializer : value mod contended portable =
      KI : 'a key * ('a -> (unit -> 'a) @ portable once) @@ portable
      -> key_initializer
  [@@unsafe_allow_any_mode_crossing "CR with-kinds"]

  type key_initializer_list = key_initializer list

  let parent_keys = Atomic.make ([] : key_initializer_list)

  let rec add_parent_key ki =
    let l = Atomic.get parent_keys in
    if not (Atomic.compare_and_set parent_keys l (ki :: l))
    then add_parent_key ki

  let new_key ?split_from_parent init_orphan =
    let idx = Atomic.fetch_and_add key_counter 1 in
    let k = idx, { Modes.Portable.portable = init_orphan } in
    begin match split_from_parent with
    | None -> ()
    | Some split -> add_parent_key (KI (k, split))
    end;
    k

  (* If necessary, grow the current domain's local state array such that [idx]
   * is a valid index in the array. *)
  let maybe_grow idx =
    let state = get_tls_state () in
    let size = Array.length state in
    if idx < size then state
    else begin
      let rec compute_new_size s =
        if idx < s then s else compute_new_size (2 * s)
      in
      let new_size = compute_new_size size in
      let new_state = Array.make new_size (Obj.magic_uncontended Obj_opt.none) in
      Array.blit state 0 new_state 0 size;
      set_tls_state new_state;
      new_state
    end

  let set (type a) (idx, _init) (x : a) =
    (* Assures [idx] is in range. *)
    let state = maybe_grow idx in
    (* [Sys.opaque_identity] ensures that flambda does not look at the type of
     * [x], which may be a [float] and conclude that the [st] is a float array.
     * We do not want OCaml's float array optimisation kicking in here. *)
    Array.unsafe_set state idx (Obj_opt.some (Sys.opaque_identity x))

  let get (type a) ((idx, init) : a key) : a =
    (* Assures [idx] is in range. *)
    let state = maybe_grow idx in
    let obj = Array.unsafe_get state idx in
    if Obj_opt.is_some obj
    then (Obj_opt.unsafe_get obj : a)
    else begin
      let v : a = init.Modes.Portable.portable () in
      let new_obj = Obj_opt.some (Sys.opaque_identity v) in
      let state = get_tls_state () in
      Array.unsafe_set state idx new_obj;
      v
    end

  type key_value : value mod portable contended =
      KV : 'a key * (unit -> 'a) @@ portable -> key_value
  [@@unsafe_allow_any_mode_crossing "CR with-kinds"]

  let get_initial_keys () : key_value list =
    List.map
      (* [v] is applied exactly once in [set_initial_keys] *)
      (fun (KI (k, split)) ->
        let v = Obj.magic_many (split (get k)) |> Obj.magic_portable in
        KV (k, v))
      (Atomic.get parent_keys : key_initializer_list)

  let set_initial_keys (l : key_value list) =
    List.iter (fun (KV (k, v)) -> set k (v ())) l

  (* Freshly spawned domains have uninitialized TLS state, so it would be
     unsafe for the split functions of DLS keys registered before now to
     use TLS. No such keys exist because we are defining the TLS module. *)
  let _split_tls_state_on_spawn =
    Domain.Safe.DLS.new_key ~split_from_parent:(fun () ->
      let tls_keys = get_initial_keys () in
      (fun () -> init (); set_initial_keys tls_keys))
      (fun () -> ())
end

type t : value mod contended portable

external thread_initialize : unit -> unit = "caml_thread_initialize"
external thread_cleanup : unit -> unit @@ portable = "caml_thread_cleanup"
external thread_new : (unit -> unit) @ once -> t @@ portable = "caml_thread_new"
external thread_uncaught_exception : exn -> unit @@ portable =
            "caml_thread_uncaught_exception"

external yield : unit -> unit @@ portable = "caml_thread_yield"
external self : unit -> t @@ portable = "caml_thread_self" [@@noalloc]
external id : t -> int @@ portable = "caml_thread_id" [@@noalloc]
external join : t -> unit @@ portable = "caml_thread_join"

(* For new, make sure the function passed to thread_new never
   raises an exception. *)

let[@inline never] check_memprof_cb () = ref ()

let default_uncaught_exception_handler = thread_uncaught_exception

let uncaught_exception_handler = Atomic.make { Modes.Portable.portable = default_uncaught_exception_handler }

let set_uncaught_exception_handler (fn @ portable) =
  Atomic.set uncaught_exception_handler { Modes.Portable.portable = fn }

exception Exit

let create (fn @ once) arg =
  let tls_keys = TLS.get_initial_keys () in
  thread_new
    (fun () ->
      TLS.init ();
      TLS.set_initial_keys tls_keys;
      try
        fn arg;
        ignore (Sys.opaque_identity (check_memprof_cb ()))
      with
      | Exit ->
        ignore (Sys.opaque_identity (check_memprof_cb ()))
      | exn ->
        let raw_backtrace = Printexc.get_raw_backtrace () in
        flush stdout; flush stderr;
        try
          (Atomic.get uncaught_exception_handler).portable exn
        with
        | Exit -> ()
        | exn' ->
          Printf.eprintf
            "Thread %d killed on uncaught exception %s\n"
            (id (self ())) (Printexc.to_string exn);
          Printexc.print_raw_backtrace stderr raw_backtrace;
          Printf.eprintf
            "Thread %d uncaught exception handler raised %s\n"
            (id (self ())) (Printexc.to_string exn');
          Printexc.print_backtrace stdout;
          flush stderr)

module Portable = struct
  let create (fn @ once portable) arg = create fn arg
end

let create (fn @ many) arg = create fn arg

let exit () =
  raise Exit

(* Initialization of the scheduler *)

let () =
  thread_initialize ();
  TLS.init ();
  (* Called back in [caml_shutdown], when the last domain exits. *)
  Callback.Safe.register "Thread.at_shutdown" thread_cleanup

(* Wait functions *)

let delay = Unix.sleepf

let wait_timed_read fd d =
  match Unix.select [fd] [] [] d with ([], _, _) -> false | (_, _, _) -> true
let wait_timed_write fd d =
  match Unix.select [] [fd] [] d with (_, [], _) -> false | (_, _, _) -> true
let select = Unix.select

let wait_pid p = Unix.waitpid [] p

external sigmask : Unix.sigprocmask_command -> int list -> int list @@ portable
   = "caml_thread_sigmask"
external wait_signal : int list -> int @@ portable = "caml_wait_signal"

external use_domains : unit -> unit @@ portable = "caml_thread_use_domains"
