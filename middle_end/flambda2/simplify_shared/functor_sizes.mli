(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Recording of functor sizes encountered during inlining decisions, for use
    with the [-dfunctor-sizes] command-line flag. *)

val record : code_id:Code_id.t -> dbg:Debuginfo.t -> size:Code_size.t -> unit

val output_then_forget : prefixname:string -> unit
