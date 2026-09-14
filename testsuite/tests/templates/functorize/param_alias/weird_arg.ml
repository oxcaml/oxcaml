(* A concrete argument whose [B] is not an alias of [A].  The scraped
   parameter interface gives [B] its own runtime field, so this is
   accepted (as an ordinary functor would) and [Uses_alias]'s
   [P_alias.B.x] reads [B]'s field, not [A]'s. *)

module Arg = struct
  module A = struct
    let x = 1
  end

  module B = struct
    let x = 2
  end
end

module Inst = Bundle_alias.Make (Arg) ()

let () = print_endline (string_of_int Inst.Uses_alias.x)
