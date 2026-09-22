(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                  *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Dune build-action tracing. Events are written only when
    [DUNE_ACTION_TRACE_DIR] is set. *)

val enabled : unit -> bool

module Json : sig
  type t =
    [ `Null
    | `False
    | `True
    | `String of string
    | `Number of string
    | `Object of (string * t) list
    | `Array of t list
    ]

  val write : t -> out_channel -> unit
end

module Event : sig
  type t

  val instant :
    ?args:(string * Json.t) list ->
    category:string -> name:string -> time_in_nanoseconds:int -> unit -> t

  val span :
    ?args:(string * Json.t) list ->
    category:string -> name:string -> start_in_nanoseconds:int ->
    finish_in_nanoseconds:int -> unit -> t
end

module Context : sig
  type t

  val create : name:string -> t
  val emit : t -> Event.t -> unit
  val close : t -> unit
end

val with_fresh_context : name:string -> f:(Context.t -> 'a) -> 'a
