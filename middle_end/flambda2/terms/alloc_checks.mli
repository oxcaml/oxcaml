(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Nathanaëlle Courant, OCamlPro                       *)
(*                                                                        *)
(*   Copyright 2026 OCamlPro SAS                                          *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** What happens to the state of the current allocation region at each of the
    ways control can leave a function call or a region.

    Invariant on allocation regions:

    At every point in the program, there is a stack of allocation regions; the
    current state is the stack of open regions. Every region primitive must
    operate on the most recent region of the stack. A region created with
    [Close] at every exit REPLACES its parent region on the stack (closing it at
    any exit also stands for closing its parent). At a join point, the stacks of
    all incoming edges must be identical (it is fine to use a region parameter
    of the join continuation to capture differing regions). When control leaves
    the function, the stack must be empty: all regions must have been closed. *)

type 'a per_exit =
  { normal : 'a;
    exn : 'a;
    notrace : 'a;
    div : 'a
  }

(** Whether an exit closes the current allocation region, or forwards it, still
    open, to the context. *)
type check =
  | Forward
  | Close

type t = check per_exit

val map : 'a per_exit -> f:('a -> 'b) -> 'b per_exit

val map2 : 'a per_exit -> 'b per_exit -> f:('a -> 'b -> 'c) -> 'c per_exit

val for_all : 'a per_exit -> f:('a -> bool) -> bool

val compare_per_exit : ('a -> 'a -> int) -> 'a per_exit -> 'a per_exit -> int

(** [Close] iff both arguments are [Close]. *)
val meet_check : check -> check -> check

val compare_check : check -> check -> int

(** Pointwise [meet_check]. *)
val meet : t -> t -> t

val print_check : Format.formatter -> check -> unit

val print_per_exit :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a per_exit -> unit

val print : Format.formatter -> t -> unit
