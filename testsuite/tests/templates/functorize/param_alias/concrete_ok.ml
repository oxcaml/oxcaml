(* A concrete argument whose [B] is a genuine alias of [A]. *)

module Arg = struct
  module A = struct
    let x = 42
  end

  module B = A
end

module Inst = Bundle_alias.Make (Arg) ()

let () = print_endline (string_of_int Inst.Uses_alias.x)
