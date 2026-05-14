[@@@ocaml.warning "+a-40-41-42"]

(** Definition of register architecture for a given backend. *)
module type T = sig
  module Reg_class : sig
    (** The "enum" representing the different classes. *)
    type t [@@immediate]

    val of_machtype : Cmm.machtype_component -> t

    val equal : t -> t -> bool

    val hash : t -> int

    val print : Format.formatter -> t -> unit

    (** The list of all classes. *)
    val all : t list
  end

  module Phys_reg : sig
    type t [@@immediate]

    include Identifiable.S with type t := t

    val reg_class : t -> Reg_class.t
  end

  val index_in_class : Phys_reg.t -> int

  val registers : Reg_class.t -> Phys_reg.t array

  val available_registers : Reg_class.t -> Phys_reg.t array

  val num_available_registers : Reg_class.t -> int

  val register_name : Cmm.machtype_component -> Phys_reg.t -> string

  val dwarf_reg_number : Cmm.machtype_component -> Phys_reg.t -> int
end

(** Definition of tables with register classes as keys. All register classes are
    always bound. *)
module type Reg_class_tbl = sig
  type reg_class

  type 'a t

  (** Creates a table by calling [f] on each and every register class to get the
      initial value for that class. *)
  val init : f:(reg_class -> 'a) -> 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val iter : 'a t -> f:(reg_class -> 'a -> unit) -> unit

  val find : 'a t -> reg_class -> 'a
end

module Make_reg_class_tbl (Regs : T) :
  Reg_class_tbl with type reg_class = Regs.Reg_class.t
