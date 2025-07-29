# 2 "backoff.mli"
(****************************************************************************)
(*                                                                          *)
(*                                 OxCaml                                   *)
(*                                                                          *)
(* Copyright (c) 2015, Théo Laurent <theo.laurent@ens.fr>                   *)
(* Copyright (c) 2015, KC Sivaramakrishnan <sk826@cl.cam.ac.uk>             *)
(* Copyright (c) 2023, Vesa Karvonen <vesa.a.j.k@gmail.com>                 *)
(*                                                                          *)
(* Permission to use, copy, modify, and/or distribute this software for any *)
(* purpose with or without fee is hereby granted, provided that the above   *)
(* copyright notice and this permission notice appear in all copies.        *)
(*                                                                          *)
(* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES *)
(* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         *)
(* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  *)
(* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   *)
(* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    *)
(* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  *)
(* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           *)
(*                                                                          *)
(****************************************************************************)

@@ portable

(** Randomized exponential backoff mechanism. *)

type t [@@immediate]
(** Type of backoff values. *)

val max_wait_log : int
(** Logarithm of the maximum allowed value for wait. *)

val create : ?lower_wait_log:int -> ?upper_wait_log:int -> unit -> t
(** [create] creates a backoff value.  [upper_wait_log], [lower_wait_log]
    override the logarithmic upper and lower bound on the number of spins
    executed by {!once}. *)

val default : t
(** [default] is equivalent to [create ()]. *)

val once : t -> t
(** [once b] executes one random wait and returns a new backoff with logarithm
    of the current maximum value incremented unless it is already at
    [upper_wait_log] of [b].

    Note that this uses the default Stdlib [Random] per-domain generator. *)

val reset : t -> t
(** [reset b] returns a backoff equivalent to [b] except with current value set
    to the [lower_wait_log] of [b]. *)

val get_wait_log : t -> int
(** [get_wait_log b] returns logarithm of the maximum value of wait for next
    {!once}. *)
