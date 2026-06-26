(** Per-function register usage information for leaf functions,
    encoded per register class for storage in .cmx files. *)

[@@@ocaml.warning "+a-40-41-42"]

(** A register set for one function: an array indexed by register class
    (using the class's [hash] function as the index), where each element
    is a bitmask of used registers within that class (bit [j] is set iff
    the register with [Regs.index_in_class = j] is clobbered). *)
type value = int array

type t

val create : unit -> t

val reset : t -> unit

(** [merge src ~into:dst] modifies [dst] by adding information from [src]. *)
val merge : t -> into:t -> unit

(** [get_value t fun_name] returns [None] if [fun_name] is not associated
    with any value. *)
val get_value : t -> string -> value option

val set_value : t -> string -> value -> unit

module Raw : sig
  type t

  (** Simple integer dump. *)
  val print : t -> unit

  (** [print_with_names ~class_name ~reg_name t] prints the register set in a
      human-readable form, using [class_name class_idx] for the name of class
      [class_idx] and [reg_name ~class_idx ~bit_idx] for the name of the
      register at bit position [bit_idx] within class [class_idx]. *)
  val print_with_names :
    class_name:(int -> string) ->
    reg_name:(class_idx:int -> bit_idx:int -> string) ->
    t ->
    unit
end

val to_raw : t -> Raw.t

val of_raw : Raw.t -> t
