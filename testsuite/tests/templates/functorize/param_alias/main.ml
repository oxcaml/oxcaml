module type S = sig
  module A : sig
    val x : int
  end

  module B = A
end

(* [S] matches the parameter's interface up to scraping, so an abstract
   argument can be forwarded into the bundle. *)
module Forward (P : S) () = struct
  module Inst = Bundle_alias.Make (P) ()

  let x = Inst.Uses_alias.x
end

module Aliased = struct
  module A = struct
    let x = 42
  end

  module B = A
end

module Forwarded = Forward (Aliased) ()

(* [B] is not an alias of [A]: accepted, and [Uses_alias]'s
   [P_alias.B.x] reads [B]'s own field. *)
module Unaliased = struct
  module A = struct
    let x = 1
  end

  module B = struct
    let x = 2
  end
end

module Direct = Bundle_alias.Make (Unaliased) ()

let () = print_endline (string_of_int Forwarded.x)
let () = print_endline (string_of_int Direct.Uses_alias.x)
