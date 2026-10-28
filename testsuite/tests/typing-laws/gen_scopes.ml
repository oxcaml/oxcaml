(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-extension laws";
 module = "gen_scopes.ml";
 ocamlopt.opt;
 flags = "-generate-laws-implementation -o gen_scopes_laws.ml";
 module = "gen_scopes.cmx";
 ocamlopt.opt;
 output = "${test_build_directory}/gen_scopes_laws.ml";
 reference = "${test_source_directory}/gen_scopes_laws.ml.reference";
 check-program-output;
 flags = "-extension laws";
 module = "gen_scopes_laws.ml";
 ocamlopt.opt;
*)

(* The laws file of a unit with laws in submodules and functors: the laws
   of a functor are scoped under the choice of its parameters. *)

module type Comparable = sig
  type t
  val compare : t -> t -> int
end

(* A law that the signature of its module hides is generated too. *)
module Partly_hidden : sig val g : int -> int end = struct
  let g x = x
  law? hidden (x : int) : g x = x
end

(* A module alias contributes no laws: the ones of [Partly_hidden] are
   generated for [Partly_hidden]. *)
module Alias = Partly_hidden

(* A functor, with a law in a submodule. *)
module Map (Key : Comparable) = struct
  type 'a t = (Key.t * 'a) list
  let empty = []
  let elem m k = List.exists (fun (k', _) -> Key.compare k k' = 0) m
  let find m k =
    Option.map snd (List.find_opt (fun (k', _) -> Key.compare k k' = 0) m)
  law? find_elem (k : Key.t) (m : 'a t) :
    not (elem m k) ===> Option.is_none (find m k)
  module Empty = struct
    law? find_empty (k : Key.t) : Option.is_none (find empty k)
  end
end

(* Two parameters. *)
module Lex (A : Comparable) (B : Comparable) = struct
  let compare (a, b) (a', b') =
    match A.compare a a' with 0 -> B.compare b b' | c -> c
  law? refl (p : A.t * B.t) : compare p p = 0
end

(* A functor application contributes no laws: the ones of [Lex] are
   generated for [Lex], under the backend's choice of its parameters. *)
module Int_cmp = struct type t = int let compare = Int.compare end
module Pairs = Lex (Int_cmp) (Int_cmp)

(* A functor in a submodule, and a functor in a functor. *)
module Order = struct
  module Max (X : Comparable) = struct
    let max a b = if X.compare a b >= 0 then a else b
    law? max_comm (a : X.t) (b : X.t) : X.compare (max a b) (max b a) = 0
    module Pair (Y : sig type t val equal : t -> t -> bool end) = struct
      law? pair_refl (a : X.t) (b : Y.t) :
        X.compare (max a a) a = 0 && Y.equal b b
    end
  end
end

(* A parameter whose signature has a law. *)
module Twice (X : sig
    val f : int -> int
    law? idem (x : int) : f (f x) = f x
  end) = struct
  let g x = X.f (X.f x)
  law? g_idem (x : int) : g (g x) = g x
end

(* A parameter with the name of the parameter of the enclosing functor. *)
module Outer (X : Comparable) = struct
  module Inner (X : Comparable) = struct
    type t = X.t
    let mk x = x
    law? mk_ok (x : X.t) : mk x = mk x
  end
end
