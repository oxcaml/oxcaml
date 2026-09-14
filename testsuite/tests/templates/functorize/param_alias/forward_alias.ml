(* Forwarding an abstract functor argument into the bundle.  [S] is
   exactly the parameter's interface, yet the application is rejected:
   [(P : S)] scrapes the alias from [P]'s type
   ([Mtype.scrape_for_functor_arg]), while [Make]'s parameter type
   retains [module B = A] verbatim from the parameter's cmi.  No
   abstract argument can ever satisfy it. *)

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
