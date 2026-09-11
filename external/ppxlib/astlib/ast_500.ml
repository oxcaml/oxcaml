(* 5.0 has the same parsetree as 4.14, but uses different magic numbers.  We also
   expose the current compiler's [Longident] shape here so OxCaml ppxes can handle it
   through [ppxlib_jane] while the rest of ppxlib keeps using its 5.2 parsetree view. *)

module Longident = struct
  include Ast_999.Longident
end

module Asttypes = struct
  include Ast_414.Asttypes
end

module Parsetree = struct
  include Ast_414.Parsetree
end

module Config = struct
  (** There is no version of the compiler with this parsetree, so these magic numbers are
      made up *)
  let ast_impl_magic_number = "Caml1999M999"
  let ast_intf_magic_number = "Caml1999N999"
end
