open Owee_buf

type attribute = private {
  name : Owee_attribute.t;
  form : Owee_form.t;
  data : u128 option
}

type abbrev = private {
  tag : Owee_tag.t;
  has_child : bool;
  attributes : attribute list
}

type t

val get : offset:int -> code:int -> t -> abbrev option
val read : Owee_elf.section array -> Owee_buf.t -> t
val iter : f:(offset:s128 -> abbrevs:abbrev U128Map.t -> unit) -> t -> unit
