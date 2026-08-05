(* The only difference between 4.14 and 5.0 from a Parsetree point of view are the magic numbers *)

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
