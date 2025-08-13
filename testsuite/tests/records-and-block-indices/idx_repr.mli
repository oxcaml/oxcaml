(** Inspect the representation of a block index, for testing and debugging *)

type t

val of_idx_imm : 'a ('b : any). ('a, 'b) idx_imm -> t

val of_idx_mut : 'a ('b : any). ('a, 'b) idx_mut -> t

val equal : t -> t -> bool

val debug_string : t -> string
