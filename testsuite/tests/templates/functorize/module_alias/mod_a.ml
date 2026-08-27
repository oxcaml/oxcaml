(* Parameters: P *)

(* Aliases [Mod_b] under -no-alias-deps; combined with mod_b.ml's symmetric
   alias, the two cmis cross-reference each other. *)

module Mod_b_alias = Mod_b

let foo (p : P.t) : string = ignore p; "foo"
