(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

(** Representation of sets of unicode code points. *)

(** Character sets are represented as lists of intervals. The intervals must be
    non-overlapping and not collapsable, and the list must be ordered in
    increasing order. *)
type t = private (int * int) list

val of_list : (int * int) list -> t
val to_list : t -> (int * int) list
val min_code : int
val max_code : int
val empty : t
val any : t
val union : t -> t -> t
val union_list : t list -> t
val difference : t -> t -> t
val intersection : t -> t -> t
val is_empty : t -> bool
val eof : t
val singleton : int -> t
val interval : int -> int -> t
val to_seq : t -> int Seq.t
