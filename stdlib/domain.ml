# 2 "domain.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       *)
(*                 Stephen Dolan, University of Cambridge                 *)
(*                   Tom Kelly, OCaml Labs Consultancy                    *)
(*                                                                        *)
(*   Copyright 2019 Indian Institute of Technology, Madras                *)
(*   Copyright 2014 University of Cambridge                               *)
(*   Copyright 2021 OCaml Labs Consultancy Ltd                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Stdlib
open Modes.Portable

[@@@ocaml.flambda_o3]

external cpu_relax : unit -> unit @@ portable = "%cpu_relax"

external runtime5 : unit -> bool @@ portable = "%runtime5"

module Obj_opt : sig @@ portable
  type t
  val some : 'a -> t
  val is_some : t -> bool
  val fresh : unit -> t array
  val grow_array : t array -> int -> int -> t array
  val compare_and_set : t array -> int -> t -> t -> bool

  (** [unsafe_get obj] may only be called safely
      if [is_some] is true.

      [unsafe_get (some v)] is equivalent to
      [Obj.obj (Obj.repr v)]. *)
  val unsafe_get : t -> 'a
end = struct
  type t = Obj.t
  let none = Obj.magic_portable (Obj.repr (ref 0))
  let fresh () = Array.make 7 (Obj.magic_uncontended none)
  let[@inline] some v =
   (* [Sys.opaque_identity] ensures that flambda does not look at the type of
    * [x], which may be a [float] and conclude that the [st] is a float array.
    * We do not want OCaml's float array optimisation kicking in here. *)
    Obj.repr (Sys.opaque_identity v)
  let[@inline] is_some obj = (obj != Obj.magic_uncontended none)
  let[@inline] unsafe_get obj = Obj.obj obj

  let[@inline never] grow_array st idx size =
    let rec compute_new_size s =
      if idx < s then s else compute_new_size (2 * s + 1)
    in
    let new_size = compute_new_size size in
    let new_st =
      Array.make new_size (Obj.magic_uncontended none)
    in
    Array.blit st 0 new_st 0 size;
    new_st

  external compare_and_set_field
    : t array -> int -> t -> t -> bool @@ portable = "%atomic_cas_field"

  let[@inline] compare_and_set st idx old new_ =
    (* In Flambda 2 there is a strict distinction between arrays and blocks. *)
    compare_and_set_field (Sys.opaque_identity st) idx old new_
end

module Runtime_4 = struct
  module DLS = struct

    let state = Obj.magic_portable (Atomic.make (Obj_opt.fresh ()))

    let init () = ()

    type 'a key = int * (unit -> 'a) Modes.Portable.t

    let key_counter = Atomic.make 0

    let new_key ?split_from_parent:_ init_orphan =
      let idx = Atomic.fetch_and_add key_counter 1 in
      (idx, { portable = init_orphan })

    (* If necessary, grow the current domain's local state array such that [idx]
    * is a valid index in the array. *)
    let[@inline] rec maybe_grow idx =
      let st = Atomic.get (Obj.magic_uncontended state) in
      let sz = Array.length st in
      if idx < sz then st
      else begin
        let new_st = Obj_opt.grow_array st idx sz in
        (* We want a implementation that is safe with respect to
          single-domain multi-threading: retry if the DLS state has
          changed under our feet.
          Note that the number of retries will be very small in
          contended scenarios, as the array only grows, with
          exponential resizing. *)
        if Atomic.compare_and_set (Obj.magic_uncontended state) st new_st
        then new_st
        else maybe_grow idx
      end

    (* Disable inlining to assure poll points are never inserted between grow
       and set, which could cause us to drop the update. *)
    let[@inline never] set (type a) (idx, _init) (x : a) =
      (* Assures [idx] is in range. *)
      let st = maybe_grow idx in
      Array.unsafe_set st idx (Obj_opt.some x)

    let[@inline never] init_idx (type a) idx old_obj (init : _ -> a) =
      let v : a = init () in
      let new_obj = Obj_opt.some v in
      (* At this point, [st] or [st.(idx)] may have been changed
        by another thread on the same domain.

        If [st] changed, it was resized into a larger value,
        we can just reuse the new value.

        If [st.(idx)] changed, we drop the current value to avoid
        letting other threads observe a 'revert' that forgets
        previous modifications. *)
      let st = Atomic.get (Obj.magic_uncontended state) in
      if Obj_opt.compare_and_set st idx old_obj new_obj
      then v
      else begin
        (* if st.(idx) changed, someone must have initialized
          the key in the meantime. *)
        let updated_obj = Array.unsafe_get st idx in
        if Obj_opt.is_some updated_obj
        then (Obj_opt.unsafe_get updated_obj : a)
        else assert false
      end

    (* Inlining is ok because it's safe to return a stale value. *)
    let[@inline] get (type a) ((idx, init) : a key) : a =
      (* Assures [idx] is in range. *)
      let st = maybe_grow idx in
      let obj = Array.unsafe_get st idx in
      if Obj_opt.is_some obj
      then (Obj_opt.unsafe_get obj : a)
      else init_idx idx obj init.portable
  end

  (******** Callbacks **********)

  (* first spawn, domain startup and at exit functionality *)
  let first_domain_spawned = Atomic.make false

  let first_spawn_function = ref (fun () -> ())

  let before_first_spawn f =
    if Atomic.get first_domain_spawned then
      raise (Invalid_argument "first domain already spawned")
    else begin
      let old_f = !first_spawn_function in
      let new_f () = old_f (); f () in
      first_spawn_function := new_f
    end

  let at_exit_key = DLS.new_key (fun () -> { portable = (fun () -> ()) })

  let at_exit f =
    let old_exit : unit -> unit = (DLS.get at_exit_key).portable in
    let new_exit () =
      (* The domain termination callbacks ([at_exit]) are run in
        last-in-first-out (LIFO) order in order to be symmetric with the domain
        creation callbacks ([at_each_spawn]) which run in first-in-fisrt-out
        (FIFO) order. *)
      f (); old_exit ()
    in
    DLS.set at_exit_key { portable = new_exit }

  let do_at_exit () =
    let f : unit -> unit = (DLS.get at_exit_key).portable in
    f ()

  (* Unimplemented functions *)
  let not_implemented () =
    failwith "Multi-domain functionality not supported in runtime4"
  type !'a t : value mod portable contended with 'a
  type id = int
  let spawn _ = not_implemented ()
  let join _ = not_implemented ()
  let get_id _ = not_implemented ()

  let self () = 0
  let is_main_domain () = true
  let recommended_domain_count () = 1
  let max_domain_count = 1
  let self_index () = 0
end

module Runtime_5 = struct
  module Raw = struct
    (* Low-level primitives provided by the runtime *)
    type t = private int

    (* The layouts of [state] and [term_sync] are hard-coded in
      [runtime/domain.c] *)

    type 'a state =
      | Running
      | Finished of ('a, exn) result [@warning "-unused-constructor"]

    type 'a term_sync : value mod portable contended with 'a = {
      (* protected by [mut] *)
      mutable state : 'a state [@warning "-unused-field"] ;
      mut : Mutex.t ;
      cond : Condition.t ;
    } [@@unsafe_allow_any_mode_crossing]

    external spawn : (unit -> 'a) @ portable once -> 'a term_sync -> t @@ portable
      = "caml_domain_spawn"
    external self : unit -> t @@ portable
      = "caml_ml_domain_id" [@@noalloc]
    external get_recommended_domain_count: unit -> int @@ portable
      = "caml_recommended_domain_count" [@@noalloc]
    external get_max_domain_count : unit -> int @@ portable
      = "caml_max_domain_count" [@@noalloc]
  end

  type id = Raw.t

  type 'a t = {
    domain : Raw.t;
    term_sync : 'a Raw.term_sync;
  }

  module DLS = struct

    type dls_state = Obj_opt.t array

    external get_dls_state : unit -> dls_state @@ portable = "%dls_get"

    external set_dls_state : dls_state -> unit @@ portable =
      "caml_domain_dls_set" [@@noalloc]

    external compare_and_set_dls_state : dls_state -> dls_state -> bool @@ portable =
      "caml_domain_dls_compare_and_set" [@@noalloc]

    let init () =
      let st = Obj_opt.fresh () in
      set_dls_state st

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
      let k = idx, { portable = init_orphan } in
      begin match split_from_parent with
      | None -> ()
      | Some split -> add_parent_key (KI (k, split))
      end;
      k

    (* If necessary, grow the current domain's local state array such that [idx]
    * is a valid index in the array. *)
    let[@inline] rec maybe_grow idx =
      let st = get_dls_state () in
      let sz = Array.length st in
      if idx < sz then st
      else begin
        let new_st = Obj_opt.grow_array st idx sz in
        (* We want a implementation that is safe with respect to
          single-domain multi-threading: retry if the DLS state has
          changed under our feet.
          Note that the number of retries will be very small in
          contended scenarios, as the array only grows, with
          exponential resizing. *)
        if compare_and_set_dls_state st new_st
        then new_st
        else maybe_grow idx
      end

    (* Disable inlining to assure poll points are never inserted between grow
       and set, which could cause us to drop the update. *)
    let[@inline never] set (type a) (idx, _init) (x : a) =
      (* Assures [idx] is in range. *)
      let st = maybe_grow idx in
      Array.unsafe_set st idx (Obj_opt.some x)

    let[@inline never] init_idx (type a) idx old_obj (init : _ -> a) =
      let v : a = init () in
      let new_obj = Obj_opt.some v in
      (* At this point, [st] or [st.(idx)] may have been changed
        by another thread on the same domain.

        If [st] changed, it was resized into a larger value,
        we can just reuse the new value.

        If [st.(idx)] changed, we drop the current value to avoid
        letting other threads observe a 'revert' that forgets
        previous modifications. *)
      let st = get_dls_state () in
      if Obj_opt.compare_and_set st idx old_obj new_obj
      then v
      else begin
        (* if st.(idx) changed, someone must have initialized
          the key in the meantime. *)
        let updated_obj = Array.unsafe_get st idx in
        if Obj_opt.is_some updated_obj
        then (Obj_opt.unsafe_get updated_obj : a)
        else assert false
      end

    (* Inlining is ok because it's safe to return a stale value. *)
    let[@inline] get (type a) ((idx, init) : a key) : a =
      (* Assures [idx] is in range. *)
      let st = maybe_grow idx in
      let obj = Array.unsafe_get st idx in
      if Obj_opt.is_some obj
      then (Obj_opt.unsafe_get obj : a)
      else init_idx idx obj init.portable

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
  end

  (******** Identity **********)

  let get_id { domain; _ } = domain

  let self () = Raw.self ()

  let is_main_domain () = (self () :> int) = 0

  external self_index : unit -> int# @@ portable
    = "%domain_index" [@@noalloc]

  external tag_int : int# -> int @@ portable = "%tag_int"

  let[@inline] self_index () = tag_int (self_index ())

  (******** Callbacks **********)

  (* first spawn, domain startup and at exit functionality *)
  let first_domain_spawned = Atomic.make false

  let first_spawn_function = Obj.magic_portable (ref (fun () -> ()))

  let before_first_spawn f =
    if Atomic.get first_domain_spawned then
      raise (Invalid_argument "first domain already spawned")
    else begin
      let old_f = !first_spawn_function in
      let new_f () = old_f (); f () in
      first_spawn_function := new_f
    end

  let do_before_first_spawn () =
    if not (Atomic.get first_domain_spawned) then begin
      Atomic.set first_domain_spawned true;
      let first_spawn_function = Obj.magic_uncontended first_spawn_function in
      !first_spawn_function ();
      (* Release the old function *)
      first_spawn_function := (fun () -> ())
    end

  let at_exit_key = DLS.new_key (fun () -> { portable = (fun () -> ()) })

  let at_exit f =
    let old_exit : unit -> unit = (DLS.get at_exit_key).portable in
    let new_exit () =
      f (); old_exit ()
    in
    DLS.set at_exit_key { portable = new_exit }

  let do_at_exit () =
    let f : unit -> unit = (DLS.get at_exit_key).portable in
    f ()

  (******* Creation and Termination ********)

  let spawn f =
    do_before_first_spawn ();
    let dls_keys = DLS.get_initial_keys () in

    (* [term_sync] is used to synchronize with the joining domains *)
    let term_sync =
      Raw.{ state = Running ;
            mut = Mutex.create () ;
            cond = Condition.create () }
    in

    let body () =
      match
        DLS.init ();
        DLS.set_initial_keys dls_keys;
        let res = f () in
        res
      with
      (* Run the [at_exit] callbacks when the domain computation either
        terminates normally or exceptionally. *)
      | res ->
          (* If the domain computation terminated normally, but the
            [at_exit] callbacks raised an exception, then return the
            exception. *)
          do_at_exit ();
          res
      | exception exn ->
          (* If both the domain computation and the [at_exit] callbacks
            raise exceptions, then ignore the exception from the
            [at_exit] callbacks and return the original exception. *)
          (try do_at_exit () with _ -> ());
          raise exn
    in
    let domain = Raw.spawn body term_sync in
    { domain ; term_sync }

  let join { term_sync ; _ } =
    let open Raw in
    let rec loop () =
      match term_sync.state with
      | Running ->
          Condition.wait term_sync.cond term_sync.mut;
          loop ()
      | Finished res ->
          res
    in
    match Mutex.protect term_sync.mut loop with
    | Ok x -> x
    | Error ex -> raise ex

  let recommended_domain_count = Raw.get_recommended_domain_count
  let max_domain_count = Raw.get_max_domain_count ()
end

module type S = sig
  module DLS : sig

    type 'a key = int * (unit -> 'a) Modes.Portable.t

    val new_key
      : ?split_from_parent:('a -> (unit -> 'a) @ portable once) @ portable
      -> (unit -> 'a) @ portable
      -> 'a key
      @@ portable

    val get : 'a key -> 'a @@ portable
    val set : 'a key -> 'a -> unit @@ portable
    val init : unit -> unit
  end

  type !'a t : value mod portable contended with 'a
  val spawn : (unit -> 'a) @ portable once -> 'a t @@ portable
  val join : 'a t -> 'a @@ portable
  type id = private int
  val get_id : 'a t -> id @@ portable
  val self : unit -> id @@ portable
  val is_main_domain : unit -> bool @@ portable
  val recommended_domain_count : unit -> int @@ portable
  val max_domain_count : int
  val self_index : unit -> int @@ portable
  val before_first_spawn : (unit -> unit) -> unit @@ nonportable
  val at_exit : (unit -> unit) @ portable -> unit @@ portable
  val do_at_exit : unit -> unit @@ nonportable
end

let runtime_4_impl = (module Runtime_4 : S)
let runtime_5_impl = (module Runtime_5 : S)

let impl = if runtime5 () then runtime_5_impl else runtime_4_impl

module M : S = (val impl)
include M

module TLS0 = struct

  type tls_state = Obj_opt.t array

  external get_tls_state
    : unit -> tls_state @@ portable = "%tls_get"
  [@@noalloc]
  external set_tls_state
    : tls_state -> unit @@ portable = "caml_domain_tls_set"
  [@@noalloc]

  let get_tls_state =
    if runtime5 ()
    then get_tls_state
    else fun [@inline] () ->
      let state = get_tls_state () in
      if Obj.is_block (Obj.repr state) then state
      else [||]

  type 'a key = 'a DLS.key

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
  let[@inline] maybe_grow idx =
    let st = get_tls_state () in
    let size = Array.length st in
    if idx < size then st
    else begin
      let new_st = Obj_opt.grow_array st idx size in
      set_tls_state new_st;
      new_st
    end

  let[@inline] set (type a) (idx, _init) (x : a) =
    (* Assures [idx] is in range. *)
    let st = maybe_grow idx in
    Array.unsafe_set st idx (Obj_opt.some x)

  let[@inline never] init_idx (type a) idx (init : _ -> a) =
    let v : a = init () in
    let new_obj = Obj_opt.some v in
    let st = get_tls_state () in
    Array.unsafe_set st idx new_obj;
    v

  let[@inline] get (type a) ((idx, init) : a key) : a =
    (* Assures [idx] is in range. *)
    let st = maybe_grow idx in
    let obj = Array.unsafe_get st idx in
    if Obj_opt.is_some obj
    then (Obj_opt.unsafe_get obj : a)
    else init_idx idx init.portable

  type key_value : value mod portable contended =
      KV : 'a key * (unit -> 'a) @@ portable -> key_value
  [@@unsafe_allow_any_mode_crossing "CR with-kinds"]

  module Private = struct
    type keys = key_value list

    let init () =
      let st = Obj_opt.fresh () in
      set_tls_state st

    let get_initial_keys () : key_value list =
      List.map
        (* [v] is applied exactly once in [set_initial_keys] *)
        (fun (KI (k, split)) ->
          let v = Obj.magic_many (split (get k)) |> Obj.magic_portable in
          KV (k, v))
        (Atomic.get parent_keys : key_initializer_list)

    let set_initial_keys (l : key_value list) =
      List.iter (fun (KV (k, v)) -> set k (v ())) l
  end
end

module Tick = struct
  module type S = sig @@ portable
    type t : mutable_data mod external_ global

    val acquire : interval_usec:int -> t @ unique
    val release : t @ unique -> unit
  end

  module Runtime4 = struct
    type t = |

    let fail () =
      failwith "[Domain.Tick] not supported in runtime4"

    let acquire ~interval_usec:_ = fail ()
    let release = function | (_ : t) -> .
  end

  module Runtime5 = struct
    module Registry : sig @@ portable
      type t : sync_data

      val create : unit -> t

      (** These two functions return the new min interval *)
      val add : t -> int -> int
      val remove : t -> int -> int or_null
    end = struct
      (* NOTE this is extremely un-optimized; we assume that ticks are acquired
         and released relatively infrequently so the performance of registry
         modification doesn't matter. *)

      module Inner = struct
        (* It would be better for this to be a decent min-heap, but there is not
           one around that is convenient to use (and see above note about this
           not being tight-loop). *)
        include Map.MakePortable (Int)

        external magic_empty_stateless
          : ('a t[@local_opt])
          -> ('a t[@local_opt]) @ stateless
          @@ stateless
          = "%identity"

        external magic_empty_read_write
          : ('a t[@local_opt]) @ immutable
          -> ('a t[@local_opt])
          @@ stateless
          = "%identity"

        let empty = magic_empty_stateless empty
        let[@inline] empty () = magic_empty_read_write empty
      end

      type t : sync_data =
        { mutable inner : int Inner.t
        (* A bag, mapping the interval to the number of requesters of ticks with
           that interval *)
        ; mutex : Mutex.t
        }
      [@@unsafe_allow_any_mode_crossing "All accesses protected by mutex"]

      let create () =
        { inner = Inner.empty ()
        ; mutex = Mutex.create ()
        }

      let protect t f = Mutex.protect t.mutex f

      let add t tick = protect t (fun () ->
        let inner' =
          Inner.update tick
            (function
              | None -> Some 1
              | Some i -> Some (i + 1))
            t.inner
        in
        t.inner <- inner';
        Inner.min_binding inner' |> fst)

      let remove t tick = protect t (fun () ->
        let inner' =
          Inner.update tick
            (function
              | None | Some 0 | Some 1 -> None
              | Some i -> Some (i - 1))
            t.inner
        in
        t.inner <- inner';
        match Inner.min_binding inner' with
        | (interval, _) -> This interval
        | exception Not_found -> Null)
    end

    (* NOTE: st_stubs.c relies on this being an int (and in particular not
       scanned) *)
    type t = int

    (* One registry per recommended_domain_count.

       If more than recommended_domain_count domains are spawned (which in
       practice we never do in OxCaml), multiple domains share a registry. This
       is fine since we have to have synchronization for systhreads anyway
    *)
    let registry =
      (* CR ocaml-5.4: This should be an iarray *)
      Array.init (recommended_domain_count ()) (fun _ -> Registry.create ())

    let local_registry () =
      (* Safety: modulo ensures this is in bounds *)
      Array.unsafe_get
        (* Safety: Array is never mutated after creation *)
        (Obj.magic_uncontended registry)
        (self_index () mod recommended_domain_count ())

    external set_tick_interval_usec
      : (int[@untagged]) -> (unit[@untagged])
      @@ portable
      = "caml_domain_set_tick_interval_usec_bytecode"
          "caml_domain_set_tick_interval_usec"

    let acquire ~interval_usec =
      let registry = local_registry () in
      let interval = Registry.add registry interval_usec in
      match set_tick_interval_usec interval with
      | () -> interval_usec
      | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        (* We might raise because we failed to start the tick thread, not just
           because the interval was invalid. In that case, we don't want to
           leave the domain's min requested tick interval in place - instead, we
           use the interval returned by [Registry.remove] to set the tick
           interval again. This will probably (though might not) fail because we
           fail to start the tick thread again; in that case, it's fine to
           re-raise that exception as the state should be the same as we left it
           before calling [acquire] *)
        (match Registry.remove registry interval_usec with
         | This interval -> set_tick_interval_usec interval
         | Null -> set_tick_interval_usec 0);
        Printexc.raise_with_backtrace exn bt

    let release interval_usec =
      let registry = local_registry () in
      (* Note that the calls to [set_tick_interval_usec] can't raise here, as
         we're neither setting the requested interval to a value we haven't
         successfully set it to before, nor are we starting the tick thread. *)
      match Registry.remove registry interval_usec with
      | Null -> set_tick_interval_usec 0
      | This interval -> set_tick_interval_usec interval

  end

  let impl =
    if runtime5 ()
    then (module Runtime5 : S)
    else (module Runtime4 : S)

  include (val impl : S)

  let () = Callback.Safe.register "Domain.Tick.acquire" acquire
  let () = Callback.Safe.register "Domain.Tick.release" release

  external local_requested_interval_usec
    : unit -> int @@ portable
    = "caml_domain_get_tick_interval_usec"
  [@@noalloc]

  external global_effective_interval_usec
    : (unit[@untagged]) -> (int[@untagged]) @@ portable
    = "caml_effective_tick_interval_usec_bytecode"
        "caml_effective_tick_interval_usec"
  [@@noalloc]
end

module Safe = struct
  (* Note the exposed signature of [get] and [set] add modes for safety. *)
  module DLS = DLS
  module TLS = TLS0

  let spawn f =
    let tls_keys = TLS.Private.get_initial_keys () in
    spawn (fun () ->
      TLS.Private.init ();
      TLS.Private.set_initial_keys tls_keys;
      f ()) [@nontail]

  let at_exit = at_exit
end

module TLS = struct
  module Private = TLS0.Private

  type 'a key = 'a TLS0.key

  let new_key ?split_from_parent f =
    let split_from_parent =
      match split_from_parent with
      | None -> None
      | Some split_from_parent ->
        Some (Obj.magic_portable (fun x ->
          Obj.magic_portable (fun () -> split_from_parent x)))
    in
    TLS0.new_key ?split_from_parent (Obj.magic_portable f)
  ;;

  let get = TLS0.get
  let set = TLS0.set
  let init = TLS0.Private.init
end

module DLS = struct
  type 'a key = 'a Safe.DLS.key

  let new_key ?split_from_parent f =
    let split_from_parent =
      match split_from_parent with
      | None -> None
      | Some split_from_parent ->
        Some (Obj.magic_portable (fun x ->
          Obj.magic_portable (fun () -> split_from_parent x)))
    in
    DLS.new_key ?split_from_parent (Obj.magic_portable f)
  ;;

  let get = DLS.get
  let set = DLS.set
  let init = DLS.init
end

let spawn f = Safe.spawn (Obj.magic_portable f)
let at_exit f = Safe.at_exit (Obj.magic_portable f)

let () = DLS.init ()
let () = TLS.init ()

let _ = Stdlib.do_domain_local_at_exit := do_at_exit
