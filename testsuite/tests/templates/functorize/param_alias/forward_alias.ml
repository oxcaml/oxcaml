(* Forwarding an abstract functor argument into the bundle.  [S] matches
   the parameter's interface up to scraping: [-as-parameter] scrapes the
   alias from the parameter interface just as [(P : S)] scrapes it from
   [P]'s type, so the application is accepted. *)

module type S = sig
  module A : sig
    val x : int
  end

  module B = A
end

module Forward (P : S) () = struct
  module Inst = Bundle_alias.Make (P) ()

  let x = Inst.Uses_alias.x
end

module Arg = struct
  module A = struct
    let x = 42
  end

  module B = A
end

module Res = Forward (Arg) ()

let () = print_endline (string_of_int Res.x)
