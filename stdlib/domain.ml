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

  let compare_and_set st idx old new_ = 
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

    let[@inline] set (type a) (idx, _init) (x : a) =
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

    let[@inline] set (type a) (idx, _init) (x : a) =
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
  module Impl = struct
    type tls_state = Obj_opt.t array

    external get_tls_state
      : unit -> tls_state @@ portable = "caml_thread_get_state"
    [@@noalloc]
    external set_tls_state
      : tls_state -> unit @@ portable = "caml_thread_set_state"
    [@@noalloc]

    let init () =
      let st = Obj_opt.fresh () in
      set_tls_state st

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

  type 'a key = 'a Impl.key

  external has_tls_state
    : unit -> bool @@ portable = "caml_thread_has_tls_state"
  let has_tls_state = has_tls_state ()

  let[@inline] new_key ?split_from_parent init = 
    if has_tls_state 
    then Impl.new_key ?split_from_parent init 
    else DLS.new_key ?split_from_parent init 
  let[@inline] get key = 
    if has_tls_state then Impl.get key else DLS.get key
  let[@inline] set key v = 
    if has_tls_state then Impl.set key v else DLS.set key v

  module Private = struct
      type keys = Impl.key_value list
      external init_main_thread 
        : unit -> unit @@ portable = "caml_thread_init_main_thread"
      let[@inline] init () = 
        if has_tls_state then Impl.init ()
      let[@inline] get_initial_keys () = 
        if has_tls_state then Impl.get_initial_keys () else []
      let[@inline] set_initial_keys keys = 
        if has_tls_state then Impl.set_initial_keys keys

      (* If TLS is supported, we need to initialize it here, since [Thread]
         might never be loaded. *)
      let[@inline] init_main_thread () = 
        if has_tls_state then (init_main_thread (); init ())
      let () = init_main_thread ()
  end
end

module Safe = struct
  (* Note the exposed signature of [get] and [set] add modes for safety. *)
  module DLS = DLS
  module TLS = struct 
    include TLS0
    let[@inline] access key f = exclave_ f (get key)
  end
  
  let spawn f = 
    let tls_keys = TLS.Private.get_initial_keys () in
    spawn (fun () -> 
      TLS.Private.init_main_thread (); 
      TLS.Private.set_initial_keys tls_keys; 
      f ()) [@nontail]

  let at_exit = at_exit
end

module TLS = struct
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

  module Private = TLS0.Private
end

let () = DLS.init ()

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
end

let spawn f = Safe.spawn (Obj.magic_portable f)
let at_exit f = Safe.at_exit (Obj.magic_portable f)

let _ = Stdlib.do_domain_local_at_exit := do_at_exit
