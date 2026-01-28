[@@@ocaml.warning "+a-40-41-42"]

(* Definition of register classes for a given backend. *)
module type T = sig
  (** The "enum" representing the different classes. *)
  type t

  module Reg_id : sig
    type t = private int

    include Identifiable.S with type t := t

    val of_int : int -> t
  end

  (** The list of all classes. *)
  val all : t list

  val reg_index_in_class : t -> Reg_id.t -> int

  val reg_id : t -> reg_index_in_class:int -> Reg_id.t

  val num_available_registers : t -> int

  val num_registers : t -> int

  (** For a given register class, the DWARF register numbering for that class.
      Given an allocated register with location [Reg n] and class [reg_class],
      the returned array contains the corresponding DWARF register number at
      index [n - first_available_register reg_class]. *)
  val dwarf_register_numbers : t -> int array

  val register_name : Cmm.machtype_component -> Reg_id.t -> string

  val equal : t -> t -> bool

  val hash : t -> int

  val print : Format.formatter -> t -> unit

  val of_machtype : Cmm.machtype_component -> t
end

(** Definition of tables with register classes as keys. All register classes are
    always bound. *)
module type Tbl = sig
  type reg_class

  type 'a t

  (** Creates a table by calling [f] on each and every register class to get the
      initial value for that class. *)
  val init : f:(reg_class -> 'a) -> 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val iter : 'a t -> f:(reg_class -> 'a -> unit) -> unit

  val find : 'a t -> reg_class -> 'a
end

module Make_tbl (RC : T) : Tbl with type reg_class = RC.t
