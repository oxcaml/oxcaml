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

type ('a : value_or_null) request_inner : value mod contended portable  =
  { action : 'a @ contended once portable unique -> unit @@ portable
  ; argument : 'a @@ contended portable
  ; mutable spawn_result : 'a spawn_result @@ contended portable
  ; mutable sender_domain_or_negative_if_spawned : int
  }
[@@unsafe_allow_any_mode_crossing
   (* The mutable fields are synchronized via a per domain [mutex] and
      [condition_spawn_result]. *)]

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

let wait_spawn_result req =
  let sender_domain_index = req.sender_domain_or_negative_if_spawned in
  if sender_domain_index >= 0 then (
    let sender_domain = get sender_domain_index in
    Mutex.lock sender_domain.mutex;
    while req.sender_domain_or_negative_if_spawned >= 0 do
      Condition.wait sender_domain.condition_spawn_result sender_domain.mutex
    done;
    Mutex.unlock sender_domain.mutex)

let wakeup_spawn_result req =
  let sender_domain_index = req.sender_domain_or_negative_if_spawned in
  let sender_domain = get sender_domain_index in
  Mutex.lock sender_domain.mutex;
  req.sender_domain_or_negative_if_spawned <- -1;
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

external magic_unique__contended_portable
  :  ('a : value_or_null).
     'a @ contended portable
  -> 'a @ contended portable unique @@ portable
  = "%identity"

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
      (match Thread.Portable.create thread request with
       | _ -> ()
       | exception exn ->
         (* This might fail if the user tries to create too many threads *)
         let bt = Printexc.get_raw_backtrace () in
         req.spawn_result <- Failed (req.argument, exn, bt);
         Atomic.Loc.decr [%atomic.loc t.threads]);
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
    ((* CR vkarvonen: This means that something else has spawned a domain using
        [Domain.spawn].

        Calling [exit] here would make this non-portable and raising here would
        mean that no error would be produced.

        What should we do in this case? *))

let n_managers = Atomic.make 0

let[@inline] create_manager i =
  let n = Atomic.get n_managers in
  if n <= i then (
    (* [n_managers] may end up being higher, but it should not overflow and
       we do not create extra domains. *)
    let n_before = ref (Atomic.fetch_and_add n_managers (i + 1 - n)) in
    while !n_before <= i do
      if !n_before = 0 then (
        assert (0 = current_domain ());
        ignore (Thread.Portable.create manager ()))
      else
        ignore (Domain.Safe.spawn manager);
      incr n_before
    done)

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
  let sender_domain_index = current_domain () in
  let req =
    { action = f;
      argument = a;
      spawn_result = Spawned;
      sender_domain_or_negative_if_spawned = sender_domain_index }
  in
  push target_domain (Request req);
  (* We have added incoming work and must wakeup the manager thread. *)
  wakeup_incoming target_domain;
  wait_spawn_result req;
  (* SAFETY: We know that if we got an error here, the thread failed to spawn
     and hence no longer has a reference to [a]. *)
  magic_unique__contended_portable req.spawn_result

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
