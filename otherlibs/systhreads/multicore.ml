# 2 "multicore.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OxCaml                                 *)
(*                                                                        *)
(*                  Vesa Karvonen, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@alert "-do_not_spawn_domains"]

type ('a : value_or_null) spawn_result =
  | Spawned
  | Failed of 'a * exn @@ aliased many * Printexc.raw_backtrace @@ aliased many

type ('a : value_or_null) request_inner : value mod contended portable =
  { action : 'a @ contended once portable unique -> unit @@ portable
  ; argument : 'a @@ contended portable
  ; mutable spawn_result : 'a spawn_result Modes.Portended.t or_null [@atomic]
  ; sender_domain : int
  }

type request : value mod contended portable =
    Request : ('a : value_or_null). 'a request_inner -> request
[@@unboxed]

type t : value mod contended portable =
  { mutable threads : int [@atomic]
  ; mutable incoming : request list [@atomic]
  (** Closures which have been requested to be run on this domain *)
  ; mutex : Mutex.t
  ; condition_incoming : Condition.t
  ; condition_spawn_result : Condition.t
  }

(* CR-someday vkarvonen: Implement an API in the runtime to directly allow
   spawning threads on a given domain without having a manager thread running
   on the domain. *)

let current_domain = Domain.self_index

let domains =
  Array.init
    (Domain.recommended_domain_count ())
    (fun i ->
      { threads = Bool.to_int (i = 0)
      ; incoming = []
      ; mutex = Stdlib.Mutex.create ()
      ; condition_incoming = Stdlib.Condition.create ()
      ; condition_spawn_result = Stdlib.Condition.create ()
      })

let[@inline] max_domains () =
  (* [domains] should be an [iarray], but we can't use [iarray] yet *)
  let domains = Obj.magic_uncontended domains in
  Array.length domains

let get i =
  (* [domains] should be an [iarray], but we can't use [iarray] yet *)
  let domains = Obj.magic_uncontended domains in
  Array.unsafe_get domains i

let wait_incoming t =
  Mutex.lock t.mutex;
  if [] == Atomic.Loc.get [%atomic.loc t.incoming]
  then Condition.wait t.condition_incoming t.mutex;
  Mutex.unlock t.mutex

let wakeup_incoming t =
  Mutex.lock t.mutex;
  Mutex.unlock t.mutex;
  Condition.broadcast t.condition_incoming

external magic_unique__contended_portable
  :  ('a : value_or_null).
     'a @ contended portable
  -> 'a @ contended portable unique @@ portable
  = "%identity"

let wait_spawn_result req =
  let sender_domain = get req.sender_domain in
  Mutex.lock sender_domain.mutex;
  let rec loop () =
    match req.spawn_result with
    | This result ->
      Mutex.unlock sender_domain.mutex;
      magic_unique__contended_portable result.portended
    | Null ->
      Condition.wait sender_domain.condition_spawn_result sender_domain.mutex;
      loop ()
  in
  loop ()

let wakeup_spawn_result req =
  let sender_domain = get req.sender_domain in
  Mutex.lock sender_domain.mutex;
  Mutex.unlock sender_domain.mutex;
  Condition.broadcast sender_domain.condition_spawn_result

let[@inline] push t request =
  let backoff = ref Backoff.default in
  while
    let before = Atomic.Loc.get [%atomic.loc t.incoming] in
    let after = request :: before in
    not (Atomic.Loc.compare_and_set [%atomic.loc t.incoming] before after)
  do
    backoff := Backoff.once !backoff
  done

(** Run some function on a new thread. *)
let thread (Request { action; argument; _ })=
  let decr () =
    let t = get (current_domain ()) in
    Atomic.Loc.decr [%atomic.loc t.threads]
  in
  (* SAFETY: We know each value is only popped from the request stack once *)
  match action (magic_unique__contended_portable argument) with
  | () -> decr ()
  | exception exn ->
    let bt = Printexc.get_raw_backtrace () in
    (* We catch unhandled exceptions in order to adjust the number of running
       threads. *)
    decr ();
    Printexc.raise_with_backtrace exn bt

(* The manager thread, one of which runs per domain *)
let rec manager_loop t =
  match Atomic.Loc.get [%atomic.loc t.incoming] with
  | [] ->
    wait_incoming t;
    manager_loop t
  | _ ->
    let requests = Atomic.Loc.exchange [%atomic.loc t.incoming] [] in
    List.iter (fun ((Request req) as request) ->
      let spawn_result =
        match Thread.Portable.create thread request with
        | _ -> Spawned
        | exception exn ->
          (* This might fail if the user tries to create too many threads *)
          let bt = Printexc.get_raw_backtrace () in
          Atomic.Loc.decr [%atomic.loc t.threads];
          Failed (req.argument, exn, bt)
      in
      req.spawn_result <- This { portended = spawn_result };
      wakeup_spawn_result req)
      requests;
    manager_loop t

let manager () =
  let i = current_domain () in
  if i < max_domains () then (
    let t = get i in
    Atomic.Loc.incr [%atomic.loc t.threads];
    manager_loop t)
  else
    let open struct
      external caml_thread_fatal_spawn_outside_multicore
        : unit -> 'a @@ portable
        = "caml_thread_fatal_spawn_outside_multicore"
    end in
    caml_thread_fatal_spawn_outside_multicore ()

let n_managers = Atomic.make 0

let[@inline never] rec create_manager_slow_path i =
  let n = Atomic.get n_managers in
  if n <= i then (
    if Atomic.compare_and_set n_managers n (n + 1) then (
      if n = 0 then (
        assert (0 = current_domain ());
        ignore (Thread.Portable.create manager ()))
      else
        ignore (Domain.Safe.spawn manager));
    create_manager_slow_path i)

let[@inline] create_manager i =
  let n = Atomic.get n_managers in
  if n <= i then create_manager_slow_path i

let spawn_on ~domain:i f a =
  if i < 0 || max_domains () <= i
  then invalid_arg "Multicore.spawn_on: invalid domain index";
  create_manager i;
  let open struct
    (* CR-someday vkarvonen: Perhaps at some point we might have a nice way to
       pass a function through a data structure such that it is statically known
       to be used only once without having to use a mutable box to do so. *)
    external magic_many__contended_portable
      :  ('a : value_or_null).
         'a @ contended once portable
      -> 'a @ contended many portable
      @@ portable
      = "%identity"
  end in
  let f = magic_many__contended_portable f in
  let a = magic_many__contended_portable a in
  let target_domain = get i in
  Atomic.Loc.incr [%atomic.loc target_domain.threads];
  let req =
    { action = f;
      argument = a;
      spawn_result = Null;
      sender_domain = current_domain () }
  in
  push target_domain (Request req);
  (* We have added incoming work and must wakeup the manager thread. *)
  wakeup_incoming target_domain;
  wait_spawn_result req

let spawn f =
  let i =
    (* We first try to deterministically use all the domains. *)
    let n = Atomic.get n_managers in
    if 0 = n && max_domains () > 1
    then 1
    else if n < max_domains ()
    then n
    else (
      (* We then try to pick a lightly loaded domain.

         Instead of expensively maintaining a priority queue, for example, we
         take random samples from the domains and pick the domain that has fewer
         threads running on it. *)
      (* CR-someday vkarvonen: Use better metrics for the load on a domain. *)
      let x = Random.int (max_domains ()) in
      let y = Random.int (max_domains ()) in
      if Atomic.Loc.get [%atomic.loc (get x).threads]
         < Atomic.Loc.get [%atomic.loc (get y).threads]
      then x
      else y)
  in
  spawn_on ~domain:i f
