module Ext_name : sig
  val ppat_effect : string
end

module To_502 : sig
  open Ast_502.Parsetree

  val encode_ppat_effect :
    loc:Location.t -> effect_:pattern -> k:pattern -> pattern_desc

  val decode_ppat_effect : loc:Location.t -> payload -> pattern * pattern
end
