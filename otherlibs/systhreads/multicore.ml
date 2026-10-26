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

type t : value mod contended portable =
  { mutable threads : int [@atomic]
  }

let current_domain = Domain.self_index

external gc_tweak_get : string -> int @@ portable = "caml_gc_tweak_get"

(* Sized by [Domain.recommended_domain_count] unless overridden by the gc
   tweak, which lets tests exercise many domains on hosts with few CPUs;
   clamped to [Domain.max_domain_count] since [spawn] creates real domains. *)
let domains =
  let n =
    match gc_tweak_get "override_max_domains_test_only_not_for_prod" with
    | 0 -> Domain.recommended_domain_count ()
    | n -> Int.min n Domain.max_domain_count
  in
  Array.init n (fun i -> { threads = Bool.to_int (i = 0) })

let[@inline] max_domains () =
  (* [domains] should be an [iarray], but we can't use [iarray] yet *)
  let domains = Obj.magic_uncontended domains in
  Array.length domains

let get i =
  (* [domains] should be an [iarray], but we can't use [iarray] yet *)
  let domains = Obj.magic_uncontended domains in
  Array.unsafe_get domains i

external magic_unique__contended_portable
  :  ('a : value_or_null).
     'a @ contended portable
  -> 'a @ contended portable unique @@ portable
  = "%identity"

(** Run some function on a new thread on domain [t]. *)
let thread t action argument =
  (* SAFETY: [spawn_on] only uses [argument] itself if this thread was not
     created. *)
  match action (magic_unique__contended_portable argument) with
  | () -> Atomic.Loc.decr [%atomic.loc t.threads]
  | exception exn ->
    let bt = Printexc.get_raw_backtrace () in
    (* We catch unhandled exceptions in order to adjust the number of running
       threads. *)
    Atomic.Loc.decr [%atomic.loc t.threads];
    Printexc.raise_with_backtrace exn bt

(* The initial thread of each domain we spawn, which keeps the domain running
   so that threads can be created on it. *)
let park ~domain =
  if current_domain () <> domain then (
    let open struct
      external caml_thread_fatal_spawn_outside_multicore
        : unit -> 'a @@ portable
        = "caml_thread_fatal_spawn_outside_multicore"
    end in
    caml_thread_fatal_spawn_outside_multicore ());
  let mutex = Mutex.create () in
  let never_signaled = Condition.create () in
  Mutex.lock mutex;
  while true do
    Condition.wait never_signaled mutex
  done

(* The number of running domains, including the initial domain. *)
let n_domains = Atomic.make 1

let spawn_domains_mutex = Mutex.create ()

let[@inline never] spawn_domains_through i =
  Mutex.protect spawn_domains_mutex (fun () ->
    while Atomic.get n_domains <= i do
      let domain = Atomic.get n_domains in
      ignore (Domain.Safe.spawn (fun () -> park ~domain));
      Atomic.set n_domains (domain + 1)
    done)

let[@inline] ensure_domain_spawned i =
  if Atomic.get n_domains <= i then spawn_domains_through i

let spawn_on ~domain:i f a =
  if i < 0 || max_domains () <= i
  then invalid_arg "Multicore.spawn_on: invalid domain index";
  ensure_domain_spawned i;
  let open struct
    (* [a] is used either by the new thread or, if creating it fails, in the
       result, but never both. *)
    external magic_many__contended_portable
      :  ('a : value_or_null).
         'a @ contended once portable
      -> 'a @ contended many portable
      @@ portable
      = "%identity"
  end in
  let a = magic_many__contended_portable a in
  let t = get i in
  Atomic.Loc.incr [%atomic.loc t.threads];
  match Thread.create_on ~domain:i (fun () -> thread t f a) () with
  | _ -> Spawned
  | exception exn ->
    (* This might fail if the user tries to create too many threads *)
    let bt = Printexc.get_raw_backtrace () in
    Atomic.Loc.decr [%atomic.loc t.threads];
    Failed (magic_unique__contended_portable a, exn, bt)

let spawn f =
  let i =
    (* We first try to deterministically use all the domains. *)
    let n = Atomic.get n_domains in
    if n < max_domains ()
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
