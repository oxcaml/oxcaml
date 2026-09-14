(* A concrete argument whose [B] is not an alias of [A].  An ordinary
   functor with the same (scraped) parameter would accept this; [Make]
   requires [B] to literally be [A]. *)

module Arg = struct
  module A = struct
    let x = 1
  end

  module B = struct
    let x = 2
  end
end

module Inst = Bundle_alias.Make (Arg) ()
