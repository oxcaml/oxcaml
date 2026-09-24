(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Pierre Chambart, OCamlPro                         *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Compiler performance recording

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

type file = string

module Counters : sig
  type t

  val create : unit -> t
  val is_empty : t -> bool
  val get : string -> t -> int
  val set : string -> int -> t -> t
  val incr : string -> t -> t
  val union : t -> t -> t
  val to_string : t -> string
end

val reset : unit -> unit
(** erase all recorded profile information *)

val record_call : ?accumulate:bool -> ?cheap:(unit -> float) -> string -> (unit -> 'a) -> 'a
(** [record_call pass f] calls [f] and records its profile information. *)

val record_call_with_counters :
  ?accumulate:bool -> counter_f:('a -> Counters.t) -> ?cheap:(unit -> float) -> string -> (unit -> 'a) -> 'a
(** [record_call_with_counters counter_f pass f] calls [f] and records its profile
    information (including counter information given by calling [counter_f] on the
    result of calling [f]) *)

val record : ?accumulate:bool -> ?cheap:(unit -> float) -> string -> ('a -> 'b) -> 'a -> 'b
(** [record pass f arg] records the profile information of [f arg] *)

val record_with_counters :
  ?accumulate:bool -> counter_f:('b -> Counters.t) -> string -> ('a -> 'b) -> 'a -> 'b
(** [record_with_counters counter_f pass f arg] records the profile information of [f arg]
  and records counter information given by calling [counter_f] on the result of [f arg] *)

val annotate_file_name : string -> string
(** Annotates profiling pass for file names. *)

val print : Format.formatter -> Clflags.profile_column list -> timings_precision:int -> unit
(** Prints the selected recorded profiling information to the formatter. *)

val output_to_csv :
Format.formatter -> Clflags.profile_column list -> timings_precision:int -> unit
(** Outputs the selected recorded profiling information in CSV format to the formatter. *)

val record_action :
  gettimeofday:(unit -> float) -> name:string -> (unit -> 'a) -> 'a
(** When Dune action tracing is enabled, record a span covering the call,
    with the complete [-dprofile] hierarchy in its [profile] argument,
    regardless of the selected profile columns. The columns are [time]
    (seconds on the pass's selected clock), [alloc], [top-heap],
    [absolute-top-heap] (all in bytes), and [counters] (an object of integer
    counts). Each row also includes [calls], the number of CPU-clock reads
    attributed to it. The span uses wall-clock time.
    Pass [Unix.gettimeofday] as the clock; compiler-libs itself does not
    depend on [Unix]. *)

(** Command line flags *)

val options_doc : string
val all_columns : Clflags.profile_column list

(** A few pass names that are needed in several places, and shared to
    avoid typos. *)

val generate : string
val transl : string
val typing : string
