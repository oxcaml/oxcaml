(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-extension laws";
 module = "gen_extension_enabled.ml";
 ocamlopt.opt;
 script = "${ocamlrun} ${ocamlsrcdir}/tools/ocamllaws extract \
   -format laws-file -o gen_extension_enabled_laws.ml \
   gen_extension_enabled.cmx";
 script;
 module = "gen_extension_enabled_laws.ml";
 ocamlopt.opt;
*)

(* With the laws extension, a unit records a laws file. This test extracts the
   file and checks that it compiles.  *)

let double x = 2 * x
let scale ~by x = by * x

law_? trivial : 1 = 1
law_? labelled (x : int) : scale ~by:2 x = double x
law_? two_assumptions (x : int) (y : int) :
  x > 0 implies_ y > 0 implies_ x + y > 0

module M = struct
  type t = int list
  let empty : t = []
  law_? in_module (xs : t) : List.length xs >= List.length empty
  module N = struct
    let size (xs : t) = List.length xs
    law_? nested (xs : t) : size xs = List.length xs
  end
end

law_? qualified (xs : M.t) : M.N.size xs >= 0
law_? from_stdlib (o : int option) (r : (int, string) result) :
  Option.is_some o = Option.is_some o && Result.is_ok r = Result.is_ok r

module C : sig
  val one : int
end = struct
  let one = 1
  law_? constrained : one = 1
end

include struct
  let two = 2
  law_? included : two = 2
end

module rec R : sig
  val three : int
end = struct
  let three = 3
  law_? recursive : R.three = 3
end

open M
law_? opened (xs : t) : N.size xs >= List.length empty

module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module Partly_hidden : sig val g : int -> int end = struct
  let g x = x
  law_? hidden (x : int) : g x = x
end

module Alias = Partly_hidden

module Map (Key : Comparable) = struct
  type 'a t = (Key.t * 'a) list
  let empty = []
  let elem m k = List.exists (fun (k', _) -> Key.compare k k' = 0) m
  let find m k =
    Option.map snd (List.find_opt (fun (k', _) -> Key.compare k k' = 0) m)
  law_? find_elem (k : Key.t) (m : 'a t) :
    not (elem m k) implies_ Option.is_none (find m k)
  module Empty = struct
    law_? find_empty (k : Key.t) : Option.is_none (find empty k)
  end
end

module Lex (A : Comparable) (B : Comparable) = struct
  let compare (a, b) (a', b') =
    match A.compare a a' with 0 -> B.compare b b' | c -> c
  law_? refl (p : A.t * B.t) : compare p p = 0
end

module Int_cmp = struct type t = int let compare = Int.compare end
module Pairs = Lex (Int_cmp) (Int_cmp)

module Order = struct
  module Max (X : Comparable) = struct
    let max a b = if X.compare a b >= 0 then a else b
    law_? max_comm (a : X.t) (b : X.t) : X.compare (max a b) (max b a) = 0
    module Pair (Y : sig type t val equal : t -> t -> bool end) = struct
      law_? pair_refl (a : X.t) (b : Y.t) :
        X.compare (max a a) a = 0 && Y.equal b b
    end
  end
end

module Twice (X : sig
    val f : int -> int
    law_? idem (x : int) : f (f x) = f x
  end) = struct
  let g x = X.f (X.f x)
  law_? g_idem (x : int) : g (g x) = g x
end

module Outer (X : Comparable) = struct
  module Inner (X : Comparable) = struct
    let mk x = x
    law_? mk_ok (x : X.t) : mk x = mk x
  end
end
