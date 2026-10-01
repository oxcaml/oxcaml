(* Laws in submodules and functors. *)

module type Comparable = sig
  type t
  val compare : t -> t -> int
end

(* A plain submodule: its laws are chosen like the top-level ones. *)
module N : sig
  val double : int -> int
  law? double_add (x : int) : double x = x + x
end

(* A functor. *)
module Map (Key : Comparable) : sig
  type 'a t
  val empty : 'a t
  val set : 'a t -> key:Key.t -> data:'a -> 'a t
  val elem : 'a t -> Key.t -> bool
  val find : 'a t -> Key.t -> 'a option
  law? find_elem (k : Key.t) (m : 'a t) :
    not (elem m k) ===> Option.is_none (find m k)
  law? set_elem (k : Key.t) (v : 'a) (m : 'a t) :
    elem (set m ~key:k ~data:v) k
  module Empty : sig
    law? find_empty (k : Key.t) : Option.is_none (find empty k)
  end
end

(* Two parameters. *)
module Lex (A : Comparable) (B : Comparable) : sig
  val compare : A.t * B.t -> A.t * B.t -> int
  law? refl (p : A.t * B.t) : compare p p = 0
end

(* A functor in a submodule, and a functor in a functor. *)
module Order : sig
  module Max (X : Comparable) : sig
    val max : X.t -> X.t -> X.t
    law? max_comm (a : X.t) (b : X.t) : X.compare (max a b) (max b a) = 0
    module Pair (Y : sig type t val equal : t -> t -> bool end) : sig
      law? pair_refl (a : X.t) (b : Y.t) :
        X.compare (max a a) a = 0 && Y.equal b b
    end
  end
end

(* A generative functor: its law is not generated. *)
module Gen () : sig
  val x : int
  law? skipped : x = x
end

(* Module types from another unit, from a submodule, and with a
   constraint, are expanded. *)
module Idem : Other.S
module Sub : sig
  module type T = sig
    val h : int -> int
    law? h_pos (x : int) : h x >= 0
  end
end
module P : Sub.T
module type S2 = sig
  type t
  val f : t -> t
  law? idem (x : t) : f (f x) = f x
end
module Q : S2 with type t = int

(* A module alias contributes no laws: the ones of [N] are generated for
   [N]. *)
module N_alias = N

(* A parameter whose signature has a law. *)
module Twice (X : sig
    val f : int -> int
    law? idem (x : int) : f (f x) = f x
  end) : sig
  val g : int -> int
  law? g_idem (x : int) : g (g x) = g x
end

(* A parameter with the name of the parameter of the enclosing functor. *)
module Outer (X : Comparable) : sig
  module Inner (X : Comparable) : sig
    type t
    val mk : X.t -> t
    law? mk_ok (x : X.t) : mk x = mk x
  end
end
