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

(* CR-someday mslater: we should move ocaml_intrinsics_kernel into the
   stdlib/runtime and use clz/ctz here. *)
let bit_index_of b =
  (* As [b] contains exactly one non-zero bit, this could directly be optimized
     using techniques described in Using de Bruijn Sequences to Index a 1 in a
     Computer Word by Leiserson, Prokop, and Randall. *)
  let i, b =
    if 32 < Sys.int_size && 1 lsl 0x20 <= b then (0x20, b lsr 0x20) else (0, b)
  in
  let i, b = if 1 lsl 0x10 <= b then (i + 0x10, b lsr 0x10) else (i, b) in
  let i, b = if 1 lsl 0x08 <= b then (i + 0x08, b lsr 0x08) else (i, b) in
  let i, b = if 1 lsl 0x04 <= b then (i + 0x04, b lsr 0x04) else (i, b) in
  let i, b = if 1 lsl 0x02 <= b then (i + 0x02, b lsr 0x02) else (i, b) in
  if 1 lsl 0x01 <= b then i + 0x01 else i

module Await_atomic_bitset = struct

    type t = int Atomic.t array

    let bits_per_word = Sys.word_size - 1

    let create n =
        let n_words = (n + (bits_per_word - 1)) / bits_per_word in
        Array.init n_words (fun _ -> Atomic.make 0)
    ;;

    let set (t : t) i v =
        (* [t] should be an [iarray], but we can't use [iarray] in stdlib yet *)
        let t = Obj.magic_uncontended t in
        let word_index = i / bits_per_word in
        let bit_index = i - (word_index * bits_per_word) in
        let bit_mask = 1 lsl bit_index in
        let word = Array.get t word_index in
        if v then Atomic.logor word bit_mask
             else Atomic.logand word (lnot bit_mask)
    ;;

    let non_linearizable_pop (t : t) =
        (* Note that with the current maximum of 128 domains on 64-bit runtime
           there would be only 3 words in our intended use case of having one
           bit per domain. That means that this loop should run very quickly. *)
        (* [t] should be an [iarray], but we can't use [iarray] in stdlib yet *)
        let t = Obj.magic_uncontended t in
        let rec words word_index backoff =
            if word_index < Array.length t
            then (
            let word = Array.unsafe_get t word_index in
            let before = Atomic.get word in
            if before <> 0
            then (
                let after = before land (before - 1) in
                match Atomic.compare_and_set word before after with
                | true ->
                  let bit_mask = before - after in
                  let bit_index = bit_index_of bit_mask in
                  let i = bit_index + (word_index * bits_per_word) in
                  This i
                | false ->
                (* We just retry from the same word.

                   To make this operation (practically) linearizable we would
                   e.g. need to store a version number in each word and
                   increment that version number on each update (or otherwise
                   ensure values are unique and we can detect ABA) and then
                   retry if we notice any word has been updated during our pass
                   through the bitset.

                   It does not seem worth the trouble to make this linearizable
                   for the use case which is to just quickly try to find an idle
                   domain, because in that use case it doesn't strictly matter
                   and the bitset is also likely to be relatively uncontended so
                   we will likely skip set bits (i.e. idle domains) very
                   rarely. *)
                words word_index (Backoff.once backoff))
            else words (word_index + 1) backoff)
            else Null
        in
        words 0 Backoff.default
    ;;
end

type 'a spawn_result =
  | Spawned
  | Failed of 'a * exn @@ aliased * Printexc.raw_backtrace @@ aliased

type 'a request_inner : value mod contended portable  =
  { action : 'a @ unique portable -> unit @@ portable
  ; argument : 'a @@ portable
  ; mutable result : 'a spawn_result @@ portable
  ; mutable ready : bool
  ; mutex : Mutex.t
  ; condition : Condition.t
  }
[@@unsafe_allow_any_mode_crossing
   (* The mutable fields are synchronized via [mutex] and [condition]. *)]

type request : value mod contended portable =
    Request : 'a request_inner -> request
[@@unboxed]

type t : value mod contended portable =
  { index : int
  ; threads : int Atomic.t
  ; incoming : request list Atomic.t
  (** Closures which have been requested to be run on this domain *)
  ; mutable domain : unit Domain.t option
  (** Handle to the underlying domain, if one is running. *)
  ; mutex : Mutex.t
  ; condition : Condition.t
  }
[@@unsafe_allow_any_mode_crossing
  (* The mutable [domain] field is synchronized via the [threads] atomic. *)]

(* CR-someday vkarvonen: Implement an API in the runtime to directly allow
   spawning threads on a given domain without having a manager thread running
   on the domain. *)

let pause_manager t =
  Mutex.lock t.mutex;
  if 1 < Atomic.get t.threads && ( == ) [] (Atomic.get t.incoming)
  then Condition.wait t.condition t.mutex;
  Mutex.unlock t.mutex
;;

let wakeup_manager t =
  Mutex.lock t.mutex;
  Mutex.unlock t.mutex;
  Condition.broadcast t.condition
;;

let domain_key = Domain.Safe.DLS.new_key (fun () -> -1)

let () =
  (* We must set the value for the initial domain, because [current_domain]
     might be called before the manager thread for the initial domain has
     started. *)
  Domain.Safe.DLS.access (fun access -> Domain.Safe.DLS.set access domain_key 0)
;;

let current_domain () =
  let i = Domain.Safe.DLS.access
    (fun access : int -> Domain.Safe.DLS.get access domain_key)
  in
  if 0 <= i
  then i
  else
    (* CR-someday vkarvonen: If this were the only library through which threads
       are spawned then this error would go away. *)
    invalid_arg "Multicore.current_domain: not called from a managed domain"
;;

let domains =
  Array.init 1
    (fun i ->
      { index = i
      ; threads = Atomic.make (Bool.to_int (i = 0) * 2)
      ; incoming = Atomic.make []
      ; domain = None
      ; mutex = Stdlib.Mutex.create ()
      ; condition = Stdlib.Condition.create ()
      })
;;

let[@inline] max_domains () =
  (* [domains] should be an [iarray], but we can't use [iarray] in stdlib yet *)
  let domains = Obj.magic_uncontended domains in
  Array.length domains

let get i =
  (* [domains] should be an [iarray], but we can't use [iarray] in stdlib yet *)
  let domains = Obj.magic_uncontended domains in
  Array.unsafe_get domains i

let[@inline] atomic_update t f =
  let[@inline] rec aux backoff =
    let old = Atomic.get t in
    let new_ = f old in
    match Atomic.compare_and_set t old new_ with
    | true -> old
    | false -> aux (Backoff.once backoff)
  in
  ignore (aux Backoff.default)
;;

let push stack x = atomic_update stack (fun s -> x :: s)

(* A set of domains which are not running a thread *)
let idle_domains = Await_atomic_bitset.create (max_domains ())

let () =
  for i = 1 to max_domains () - 1 do
    Await_atomic_bitset.set idle_domains i true
  done
;;

external magic_unique__portable
  : 'a @ portable -> 'a @ portable unique @@ portable
  = "%identity"

(** Run some function on a new thread. *)
let thread (Request { action; argument; _ })=
  let decr () =
    let t = get (current_domain ()) in
    let threads_before_decr = Atomic.fetch_and_add t.threads (-1) in
    if threads_before_decr = 2
    then
      (* Only the manager thread was running on the domain in addition to us.
         We must wakeup the manager to potentially allow it to exit. *)
      wakeup_manager t
  in
  (* SAFETY: We know each value is only popped from the request stack once *)
  match action (magic_unique__portable argument) with
  | () -> decr ()
  | exception exn ->
    let bt = Printexc.get_raw_backtrace () in
    (* We catch unhandled exceptions in order to adjust the number of running
       threads. *)
    decr ();
    Printexc.raise_with_backtrace exn bt
;;

(* The manager thread, one of which runs per domain *)
let rec manager_loop t =
  let threads = Atomic.get t.threads in
  if threads = 1
  then (
    (* We are the only thread running on the domain so we try to exit. This is
       not a pool. *)
    match Atomic.compare_and_set t.threads threads (threads - 1) with
    | true -> Await_atomic_bitset.set idle_domains t.index true
    | false -> manager_loop t)
  else (
    match Atomic.get t.incoming with
    | [] ->
      pause_manager t;
      manager_loop t
    | _ ->
      let requests = Atomic.exchange t.incoming [] in
      List.iter (fun ((Request req) as request) ->
        (match Thread.Portable.create thread request with
         | _ -> ()
         | exception exn ->
           (* This might fail if the user tries to create too many threads *)
           let bt = Printexc.get_raw_backtrace () in
           (* The only exception raised by [Thread.Portable.create] is
              [Sys_error of string] which is immutable data and we only use the
              [exn] and [bt] to reraise to the caller of [spawn_on] in that
              function. In other words, the [exn] and [bt] are in fact portable
              and uncontended meaning that they do not contain shared mutable
              state being potentially accessed by multiple threads. *)
           req.result <-
             Failed (
               req.argument, Obj.magic_portable exn, Obj.magic_portable bt));
        Mutex.lock req.mutex;
        req.ready <- true;
        Condition.signal req.condition;
        Mutex.unlock req.mutex)
        requests;
      manager_loop t)
;;

let manager (t : t) =
  Await_atomic_bitset.set idle_domains t.index false;
  Domain.Safe.DLS.access
    (fun access -> Domain.Safe.DLS.set access domain_key t.index);
  manager_loop t
;;

let create_initial_manager =
  let need_manager = Atomic.make true in
  fun () ->
    if Atomic.Contended.get need_manager &&
       Atomic.Contended.exchange need_manager false
    then ignore (Thread.Portable.create manager (get 0))

let spawn_on ~domain:i f a =
  if i < 0 || max_domains () <= i
  then invalid_arg "Multicore.spawn_on: invalid domain index";
  create_initial_manager ();
  let f =
    let open struct
      (* CR-someday vkarvonen: Perhaps at some point we might have a nice way
         to pass a function through a data structure such that it is statically
         known to be used only once without having to use a mutable box to do
         so. *)
      external magic_many
        :  'a @ once portable
        -> 'a @ many portable
        @@ portable
        = "%identity"
    end in
    magic_many f
  in
  let t = get i in
  let threads_before_incr = Atomic.fetch_and_add t.threads 1 in
  let mutex = Mutex.create () in
  let condition = Condition.create () in
  let request_inner =
    { action = f; argument = a; result = Spawned; ready = false; mutex;
      condition }
  in
  let request = Request request_inner in
  push t.incoming request;
  if threads_before_incr = 0
  then (
    (* At this point it is our responsibility to spawn a domain to run the
       manager thread on it.

       We increment threads by two -- one for the manager thread and one to
       prevent the manager thread from exiting before we have stored the domain
       handle of the manager thread. *)
    let _ : int = Atomic.fetch_and_add t.threads 2 in
    let old = t.domain in
    (* We must join with the previous manager domain, if any. Otherwise it would
       be possible to attempt to start too many domains. *)
    if Option.is_some old then Domain.join (Option.get old);
    t.domain <- Some (Domain.Safe.spawn (fun () -> manager t));
    (* We have successfully stored the domain handle and now decrement [threads]
       to allow the manager thread to exit. *)
    Atomic.decr t.threads);
  (* We have added incoming work and must wakeup the manager thread. *)
  wakeup_manager t;
  Mutex.lock mutex;
  while not request_inner.ready do
    Condition.wait condition mutex
  done;
  Mutex.unlock mutex;
  (* SAFETY: We know that if we got an error here, the thread failed to spawn
     and hence no longer has a reference to [a] *)
  magic_unique__portable request_inner.result
;;

let spawn f =
  let i =
    (* We first try and see if there are idle domains. *)
    match Await_atomic_bitset.non_linearizable_pop idle_domains with
    | This i -> i
    | Null ->
      (* Instead of expensively maintaining a priority queue, for example, we
         take random samples from the domains and pick the domain that has fewer
         threads running on it. *)
      (* CR-someday vkarvonen: Use better metrics for the load on a domain. *)
      let x = Random.int (max_domains ()) in
      let y = Random.int (max_domains ()) in
      let i =
        if Atomic.get (get x).threads < Atomic.get (get y).threads then x else y
      in
      (* We check again if there are idle domains. *)
      (match Await_atomic_bitset.non_linearizable_pop idle_domains with
       | Null ->
         (* Note that we don't really care about cases where
            [non_linearizable_pop] might return [Null] even when there is a bit
            set. We just want to make a good enough decision on which domain to
            spawn to. *)
         i
       | This i -> i)
  in
  spawn_on ~domain:i f
;;
