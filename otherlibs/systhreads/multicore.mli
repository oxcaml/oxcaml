# 2 "multicore.mli"
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

@@ portable

(** [Multicore] allows spawning preemptively scheduled threads to run on domains
    running in parallel. *)

(** [max_domains ()] is the maximum number of domains. Domains are indexed from
    [0] to [max_domains () - 1]. *)
val max_domains : unit -> int

(** [current_domain ()] is the index of the domain that the current thread is
    running on.

    @raise Invalid_argument
      in case [current_domain ()] is not called from the initial domain or one
      of the other domains managed by this library. *)
val current_domain : unit -> int

type 'a spawn_result =
  | Spawned
  | Failed of 'a * exn @@ aliased * Printexc.raw_backtrace @@ aliased

(** [spawn_on ~domain action] spawns [action] as a thread running on the
    specified [domain].

    The [action] should handle all exceptions. In case the action raises an
    exception, it will be allowed to propagate and the uncaught exception
    handler will be called.

    @raise Invalid_argument
      in case the [domain] index is less than [0] or greater than
      [max_domains () - 1].

    @raise Sys_error in case the system fails to create a new thread. *)
val spawn_on
  :   domain:int
  -> ('a @ unique portable -> unit) @ once portable unyielding
  -> 'a @ unique portable
  -> 'a spawn_result @ unique portable

(** [spawn action] spawns [action] as a thread running on some domain.

    The [action] should handle all exceptions. In case the action raises an
    exception, it will be allowed to propagate and the uncaught exception
    handler will be called.

    The domain used to spawn the thread on is selected semi-probabilistically
    so as to balance the load per domain.

    @raise Sys_error in case the system fails to create a new thread. *)
val spawn
  :  ('a @ unique portable -> unit) @ once portable unyielding
  -> 'a @ unique portable
  -> 'a spawn_result @ unique portable
