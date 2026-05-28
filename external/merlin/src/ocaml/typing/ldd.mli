(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jules Jacobs, Jane Street                             *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module type Ordered = Ldd_intf.Ordered

(** Lattice polynomial terms built from joins, meets, constants, and
    variables. The interface supports least and greatest fixpoint solving
    over these terms. *)
module Make (V : Ordered) : sig
  include Ldd_intf.S with module Name = V
end
