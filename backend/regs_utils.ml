[@@@ocaml.warning "+a-40-41-42"]

module List = ListLabels

module type T = sig
  module Reg_class : sig
    type t [@@immediate]

    val of_machtype : Cmm.machtype_component -> t

    val equal : t -> t -> bool

    val hash : t -> int

    val print : Format.formatter -> t -> unit

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

module type Reg_class_tbl = sig
  type reg_class

  type 'a t

  val init : f:(reg_class -> 'a) -> 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val iter : 'a t -> f:(reg_class -> 'a -> unit) -> unit

  val find : 'a t -> reg_class -> 'a
end

module Make_reg_class_tbl (Regs : T) :
  Reg_class_tbl with type reg_class = Regs.Reg_class.t = struct
  module Tbl = Hashtbl.Make (Regs.Reg_class)

  type reg_class = Regs.Reg_class.t

  type 'a t = 'a Tbl.t

  let init : f:(reg_class -> 'a) -> 'a t =
   fun ~f ->
    let res = Tbl.create (List.length Regs.Reg_class.all) in
    List.iter Regs.Reg_class.all ~f:(fun reg_class ->
        Tbl.replace res reg_class (f reg_class));
    res

  let map : 'a t -> f:('a -> 'b) -> 'b t =
   fun tbl ~f ->
    let res = Tbl.create (List.length Regs.Reg_class.all) in
    List.iter Regs.Reg_class.all ~f:(fun reg_class ->
        Tbl.replace res reg_class (f (Tbl.find tbl reg_class)));
    res

  let iter : 'a t -> f:(reg_class -> 'a -> unit) -> unit =
   fun tbl ~f -> Tbl.iter f tbl

  let find : 'a t -> reg_class -> 'a =
   fun tbl reg_class -> Tbl.find tbl reg_class
end
