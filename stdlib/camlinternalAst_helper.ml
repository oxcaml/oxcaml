(* The single [Ast_helper] value used by the copy of [Pprintast] linked into the
   standard library for runtime metaprogramming. [varify_constructors] is only
   used on a niche poly-type printing path; copying the real one would drag in
   [Syntaxerr], so it is stubbed as the identity (generated code does not rely on
   the varification). *)

module Typ = struct
  let varify_constructors _var_names t = t
end
