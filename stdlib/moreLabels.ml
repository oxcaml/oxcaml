# 2 "moreLabels.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Jacques Garrigue, Kyoto University RIMS                 *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Stdlib

[@@@ocaml.flambda_o3]

(* Module [MoreLabels]: meta-module for compatibility labelled libraries *)

[@@@ocaml.nolabels]

(* These are 'struct include ... end' so that ocamldep picks up on the
   fact that they are not module aliases, since the mli re-exports
   them with a different signature, so they have a cmx dependency *)

module Hashtbl = struct include Hashtbl end

module Map = struct include Map end

module Set = struct include Set end
