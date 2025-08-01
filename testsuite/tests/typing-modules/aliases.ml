(* TEST
 expect;
*)

module C = Char;;
C.chr 66;;

module C' : module type of Char = C;;
C'.chr 66;;

module C3 = struct include Char end;;
C3.chr 66;;
[%%expect{|
module C = Char
- : char = 'B'
module C' :
  sig
    external code : char -> int @@ portable = "%identity"
    val chr : int -> char @@ portable
    val escaped : char -> string @@ portable
    val lowercase_ascii : char -> char @@ portable
    val uppercase_ascii : char -> char @@ portable
    type t = char
    val compare : t -> t -> int @@ portable
    val equal : t -> t -> bool @@ portable
    val seeded_hash : int -> t -> int @@ portable
    val hash : t -> int @@ portable
    external unsafe_chr : int -> char @@ portable = "%identity"
  end
- : char = 'B'
module C3 :
  sig
    external code : char -> int = "%identity"
    val chr : int -> char
    val escaped : char -> string
    val lowercase_ascii : char -> char
    val uppercase_ascii : char -> char
    type t = char
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val seeded_hash : int -> t -> int
    val hash : t -> int
    external unsafe_chr : int -> char = "%identity"
  end
- : char = 'B'
|}];;

let f x = let module M = struct module L = List end in M.L.length x;;
let g x = let module L = List in L.length (L.map succ x);;
[%%expect{|
val f : 'a list -> int = <fun>
val g : int list -> int = <fun>
|}];;

module F(X:sig end) = Char;;
module C4 = F(struct end);;
C4.chr 66;;
[%%expect{|
module F :
  functor (X : sig end) ->
    sig
      external code : char -> int @@ portable = "%identity"
      val chr : int -> char @@ portable
      val escaped : char -> string @@ portable
      val lowercase_ascii : char -> char @@ portable
      val uppercase_ascii : char -> char @@ portable
      type t = char
      val compare : t -> t -> int @@ portable
      val equal : t -> t -> bool @@ portable
      val seeded_hash : int -> t -> int @@ portable
      val hash : t -> int @@ portable
      external unsafe_chr : int -> char @@ portable = "%identity"
    end
module C4 :
  sig
    external code : char -> int @@ portable = "%identity"
    val chr : int -> char @@ portable
    val escaped : char -> string @@ portable
    val lowercase_ascii : char -> char @@ portable
    val uppercase_ascii : char -> char @@ portable
    type t = char
    val compare : t -> t -> int @@ portable
    val equal : t -> t -> bool @@ portable
    val seeded_hash : int -> t -> int @@ portable
    val hash : t -> int @@ portable
    external unsafe_chr : int -> char @@ portable = "%identity"
  end
- : char = 'B'
|}];;

module G(X:sig end) = struct module M = X end;; (* does not alias X *)
module M = G(struct end);;
[%%expect{|
module G : functor (X : sig end) -> sig module M : sig end end
module M : sig module M : sig end end
|}];;

module M' = struct
  module N = struct let x = 1 end
  module N' = N
end;;
M'.N'.x;;
[%%expect{|
module M' : sig module N : sig val x : int end module N' = N end
- : int = 1
|}];;

module M'' : sig module N' : sig val x : int end end = M';;
M''.N'.x;;
module M2 = struct include M' end;;
module M3 : sig module N' : sig val x : int end end = struct include M' end;;
M3.N'.x;;
module M3' : sig module N' : sig val x : int end end = M2;;
M3'.N'.x;;
[%%expect{|
module M'' : sig module N' : sig val x : int end end
- : int = 1
module M2 : sig module N = M'.N module N' = N end
module M3 : sig module N' : sig val x : int end end
- : int = 1
module M3' : sig module N' : sig val x : int end end
- : int = 1
|}];;

module M4 : sig module N' : sig val x : int end end = struct
  module N = struct let x = 1 end
  module N' = N
end;;
M4.N'.x;;
[%%expect{|
module M4 : sig module N' : sig val x : int end end
- : int = 1
|}];;

module F(X:sig end) = struct
  module N = struct let x = 1 end
  module N' = N
end;;
module G : functor(X:sig end) -> sig module N' : sig val x : int end end = F;;
module M5 = G(struct end);;
M5.N'.x;;
[%%expect{|
module F :
  functor (X : sig end) ->
    sig module N : sig val x : int end module N' = N end
module G : functor (X : sig end) -> sig module N' : sig val x : int end end
module M5 : sig module N' : sig val x : int end end
- : int = 1
|}];;

module M = struct
  module D = struct let y = 3 end
  module N = struct let x = 1 end
  module N' = N
end;;

module M1 : sig module N : sig val x : int end module N' = N end = M;;
M1.N'.x;;
module M2 : sig module N' : sig val x : int end end =
  (M : sig module N : sig val x : int end module N' = N end);;
M2.N'.x;;

open M;;
N'.x;;
[%%expect{|
module M :
  sig
    module D : sig val y : int end
    module N : sig val x : int end
    module N' = N
  end
module M1 : sig module N : sig val x : int end module N' = N end
- : int = 1
module M2 : sig module N' : sig val x : int end end
- : int = 1
- : int = 1
|}];;

module M = struct
  module C = Char
  module C' = C
end;;
module M1
  : sig module C : sig val escaped : char -> string end module C' = C end
  = M;; (* sound, but should probably fail *)
M1.C'.escaped 'A';;
module M2 : sig module C' : sig val chr : int -> char end end =
  (M : sig module C : sig val chr : int -> char end module C' = C end);;
M2.C'.chr 66;;
[%%expect{|
module M : sig module C = Char module C' = C end
module M1 :
  sig module C : sig val escaped : char -> string end module C' = C end
- : string = "A"
module M2 : sig module C' : sig val chr : int -> char end end
- : char = 'B'
|}];;

StdLabels.List.map;;
[%%expect{|
- : f:('a -> 'b) -> 'a list -> 'b list = <fun>
|}];;

module Q = Queue;;
exception QE = Q.Empty;;
try Q.pop (Q.create ()) with QE -> "Ok";;
[%%expect{|
module Q = Queue
exception QE
- : string = "Ok"
|}];;

module type Complex = module type of Complex with type t = Complex.t;;
module M : sig module C : Complex end = struct module C = Complex end;;

module C = Complex;;
C.one.Complex.re;;
include C;;
[%%expect{|
module type Complex =
  sig
    type t = Complex.t = { re : float; im : float; }
    val zero : t @@ portable
    val one : t @@ portable
    val i : t @@ portable
    val neg : t -> t @@ portable
    val conj : t -> t @@ portable
    val add : t -> t -> t @@ portable
    val sub : t -> t -> t @@ portable
    val mul : t -> t -> t @@ portable
    val inv : t -> t @@ portable
    val div : t -> t -> t @@ portable
    val sqrt : t -> t @@ portable
    val norm2 : t -> float @@ portable
    val norm : t -> float @@ portable
    val arg : t -> float @@ portable
    val polar : float -> float -> t @@ portable
    val exp : t -> t @@ portable
    val log : t -> t @@ portable
    val pow : t -> t -> t @@ portable
  end
module M : sig module C : Complex end
module C = Complex
- : float = 1.
type t = Complex.t = { re : float; im : float; }
val zero : t = {re = 0.; im = 0.}
val one : t = {re = 1.; im = 0.}
val i : t = {re = 0.; im = 1.}
val neg : t -> t = <fun>
val conj : t -> t = <fun>
val add : t -> t -> t = <fun>
val sub : t -> t -> t = <fun>
val mul : t -> t -> t = <fun>
val inv : t -> t = <fun>
val div : t -> t -> t = <fun>
val sqrt : t -> t = <fun>
val norm2 : t -> float = <fun>
val norm : t -> float = <fun>
val arg : t -> float = <fun>
val polar : float -> float -> t = <fun>
val exp : t -> t = <fun>
val log : t -> t = <fun>
val pow : t -> t -> t = <fun>
|}];;

module F(X:sig module C = Char end) = struct module C = X.C end;;
[%%expect{|
module F : functor (X : sig module C = Char end) -> sig module C = Char end
|}];;

(* Applicative functors *)
module S = String
module StringSet = Set.Make(String)
module SSet = Set.Make(S);;
let f (x : StringSet.t) = (x : SSet.t);;
[%%expect{|
module S = String
module StringSet :
  sig
    type elt = String.t
    type t = Set.Make(String).t
    val empty : t
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
    val map : (elt -> elt) -> t -> t
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val split : elt -> t -> t * bool * t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val subset : t -> t -> bool
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val to_list : t -> elt list
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val to_rev_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
  end
module SSet :
  sig
    type elt = S.t
    type t = Set.Make(S).t
    val empty : t
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
    val map : (elt -> elt) -> t -> t
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val split : elt -> t -> t * bool * t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val subset : t -> t -> bool
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val to_list : t -> elt list
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val to_rev_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
  end
val f : StringSet.t -> SSet.t = <fun>
|}];;

(* Also using include (cf. Leo's mail 2013-11-16) *)
module F (M : sig end) : sig type t end = struct type t = int end
module T = struct
  module M = struct end
  include F(M)
end;;
include T;;
let f (x : t) : T.t = x ;;
[%%expect{|
module F : functor (M : sig end) -> sig type t end
module T : sig module M : sig end type t = F(M).t end
module M = T.M
type t = F(M).t
val f : t -> T.t = <fun>
|}];;

(* PR#4049 *)
(* This works thanks to abbreviations *)
module A = struct
  module B = struct type t let compare x y = 0 end
  module S = Set.Make(B)
  let empty = S.empty
end
module A1 = A;;
A1.empty = A.empty;;
[%%expect{|
module A :
  sig
    module B : sig type t val compare : 'a -> 'b -> int end
    module S :
      sig
        type elt = B.t
        type t = Set.Make(B).t
        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
        val map : (elt -> elt) -> t -> t
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val split : elt -> t -> t * bool * t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val subset : t -> t -> bool
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val to_list : t -> elt list
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end
    val empty : S.t
  end
module A1 = A
- : bool = true
|}];;

(* PR#3476: *)
module FF(X : sig end) = struct type t end
module M = struct
  module X = struct end
  module Y = FF (X)
  type t = Y.t
end
module F (Y : sig type t end) (M : sig type t = Y.t end) = struct end;;

module G = F (M.Y);;
module N = G (M);;
module N = F (M.Y) (M);;
[%%expect{|
module FF : functor (X : sig end) -> sig type t end
module M :
  sig module X : sig end module Y : sig type t = FF(X).t end type t = Y.t end
module F : functor (Y : sig type t end) (M : sig type t = Y.t end) -> sig end
module G : functor (M : sig type t = M.Y.t end) -> sig end
module N : sig end
module N : sig end
|}];;

(* PR#5058 *)
module F (M : sig end) : sig type t end = struct type t = int end
module T = struct
module M = struct end
include F(M)
end
include T
let f (x : t) : T.t = x
[%%expect {|
module F : functor (M : sig end) -> sig type t end
module T : sig module M : sig end type t = F(M).t end
module M = T.M
type t = F(M).t
val f : t -> T.t = <fun>
|}]

(* PR#6307 *)

module A1 = struct end
module A2 = struct end
module L1 = struct module X = A1 end
module L2 = struct module X = A2 end;;

module F (L : (module type of L1 [@remove_aliases])) = struct end;;

module F1 = F(L1);; (* ok *)
module F2 = F(L2);; (* should succeed too *)
[%%expect{|
module A1 : sig end
module A2 : sig end
module L1 : sig module X = A1 end
module L2 : sig module X = A2 end
module F : functor (L : sig module X : sig end @@ stateless end) -> sig end
module F1 : sig end
module F2 : sig end
|}];;

(* Counter example: why we need to be careful with PR#6307 *)
module Int = struct type t = int let compare = compare end
module SInt = Set.Make(Int)
type (_,_) eq = Eq : ('a,'a) eq
type wrap = W of (SInt.t, SInt.t) eq

module M = struct
  module I = Int
  type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(I).t) eq
end;;
module type S = module type of M [@remove_aliases];; (* keep alias *)

module Int2 = struct type t = int let compare x y = compare y x end;;
module type S' = sig
  module I = Int2
  include S with module I := I
end;; (* fail *)
[%%expect{|
module Int : sig type t = int val compare : 'a -> 'a -> int end
module SInt :
  sig
    type elt = Int.t
    type t = Set.Make(Int).t
    val empty : t
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
    val map : (elt -> elt) -> t -> t
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val split : elt -> t -> t * bool * t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val subset : t -> t -> bool
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val to_list : t -> elt list
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val to_rev_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
  end
type (_, _) eq = Eq : ('a, 'a) eq
type wrap = W of (SInt.t, SInt.t) eq
module M :
  sig
    module I = Int
    type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(I).t) eq
  end
module type S =
  sig
    module I = Int @@ portable
    type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(I).t) eq
  end
module Int2 : sig type t = int val compare : 'a -> 'a -> int end
Line 15, characters 10-30:
15 |   include S with module I := I
               ^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "I"
       does not match its original definition in the constrained signature:
       Modules do not match: (module Int2) is not included in (module Int)
|}];;

(* (* if the above succeeded, one could break invariants *)
module rec M2 : S' = M2;; (* should succeed! (but this is bad) *)

let M2.W eq = W Eq;;

let s = List.fold_right SInt.add [1;2;3] SInt.empty;;
module SInt2 = Set.Make(Int2);;
let conv : type a b. (a,b) eq -> a -> b = fun Eq x -> x;;
let s' : SInt2.t = conv eq s;;
SInt2.elements s';;
SInt2.mem 2 s';; (* invariants are broken *)
*)

(* Check behavior with submodules *)
module M = struct
  module N = struct module I = Int end
  module P = struct module I = N.I end
  module Q = struct
    type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(P.I).t) eq
  end
end;;
module type S = module type of M [@remove_aliases];;
[%%expect{|
module M :
  sig
    module N : sig module I = Int end
    module P : sig module I = N.I end
    module Q :
      sig type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(P.I).t) eq end
  end
module type S =
  sig
    module N : sig module I = Int @@ portable end
    module P : sig module I = N.I @@ portable end
    module Q :
      sig type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(P.I).t) eq end
      @@ stateless
  end
|}];;

module M = struct
  module N = struct module I = Int end
  module P = struct module I = N.I end
  module Q = struct
    type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(N.I).t) eq
  end
end;;
module type S = module type of M [@remove_aliases];;
[%%expect{|
module M :
  sig
    module N : sig module I = Int end
    module P : sig module I = N.I end
    module Q :
      sig type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(N.I).t) eq end
  end
module type S =
  sig
    module N : sig module I = Int @@ portable end
    module P :
      sig
        module I : sig type t = int val compare : 'a -> 'a -> int end @@
          portable
      end
    module Q :
      sig type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(N.I).t) eq end
      @@ stateless
  end
|}];;

(* PR#6365 *)
module type S = sig module M : sig type t val x : t end end;;
module H = struct type t = A let x = A end;;
module H' = H;;
module type S' = S with module M = H';; (* shouldn't introduce an alias *)
[%%expect{|
module type S = sig module M : sig type t val x : t end end
module H : sig type t = A val x : t end
module H' = H
module type S' = sig module M : sig type t = H.t = A val x : t end end
|}];;

(* PR#6376 *)
module type Alias = sig module N : sig end module M = N end;;
module F (X : sig end) = struct type t end;;
module type A = Alias with module N := F(List);;
module rec Bad : A = Bad;;
[%%expect{|
module type Alias = sig module N : sig end module M = N end
module F : functor (X : sig end) -> sig type t end
Line 1:
Error: Module type declarations do not match:
         module type A = sig module M = F(List) end
       does not match
         module type A = sig module M = F(List) end
       At position "module type A = <here>"
       Module types do not match:
         sig module M = F(List) end
       is not equal to
         sig module M = F(List) end
       At position "module type A = sig module M : <here> end"
       Module "F(List)" cannot be aliased
|}];;

(* Shinwell 2014-04-23 *)
module B = struct
 module R = struct
   type t = string
 end

 module O = R
end

module K = struct
 module E = B
 module N = E.O
end;;

let x : K.N.t = "foo";;
[%%expect{|
module B : sig module R : sig type t = string end module O = R end
module K : sig module E = B module N = E.O end
val x : K.N.t = "foo"
|}];;

(* PR#6465 *)

module M = struct type t = A module B = struct type u = B end end;;
module P : sig type t = M.t = A module B = M.B end = M;;
module P : sig type t = M.t = A module B = M.B end = struct include M end;;
[%%expect{|
module M : sig type t = A module B : sig type u = B end end
module P : sig type t = M.t = A module B = M.B end
module P : sig type t = M.t = A module B = M.B end
|}];;

module type S = sig
  module M : sig module P : sig end end
  module Q = M
end;;
[%%expect{|
module type S = sig module M : sig module P : sig end end module Q = M end
|}];;
module type S = sig
  module M : sig module N : sig end module P : sig end end
  module Q : sig module N = M.N module P = M.P end
end;;
module R = struct
  module M = struct module N = struct end module P = struct end end
  module Q = M
end;;
module R' : S = R;;
[%%expect{|
module type S =
  sig
    module M : sig module N : sig end module P : sig end end
    module Q : sig module N = M.N module P = M.P end
  end
module R :
  sig
    module M : sig module N : sig end module P : sig end end
    module Q = M
  end
module R' : S
|}];;

module F (X : sig end) = struct type t end;;
module M : sig
  type a
  module Foo : sig
    module Bar : sig end
    type b = a
  end
end = struct
  module Foo = struct
    module Bar = struct end
    type b = F(Bar).t
  end
  type a = Foo.b
end;;
[%%expect{|
module F : functor (X : sig end) -> sig type t end
module M :
  sig type a module Foo : sig module Bar : sig end type b = a end end
|}];;

(* PR#6578 *)

module M = struct let f x = x end
module rec R : sig module M : sig val f : 'a -> 'a end end =
  struct module M = M end;;
R.M.f 3;;
[%%expect{|
module M : sig val f : 'a -> 'a end
module rec R : sig module M : sig val f : 'a -> 'a end end
- : int = 3
|}];;
module rec R : sig module M = M end = struct module M = M end;;
R.M.f 3;;
[%%expect{|
module rec R : sig module M = M end
- : int = 3
|}];;

module M = struct type t end
module type S = sig module N = M val x : N.t end
module type T = S with module N := M;;
[%%expect{|
module M : sig type t end
module type S = sig module N = M val x : N.t end
module type T = sig val x : M.t end
|}];;

(* CR zqian: the following should pass with module mode crossing *)
module X = struct module N = struct end end
module Y : sig
  module type S = sig module N = X.N end
end = struct
  module type S = module type of struct include X end
end;;
[%%expect{|
module X : sig module N : sig end end
Lines 4-6, characters 6-3:
4 | ......struct
5 |   module type S = module type of struct include X end
6 | end..
Error: Signature mismatch:
       Modules do not match:
         sig module type S = sig module N = X.N @@ stateless end end
       is not included in
         sig module type S = sig module N = X.N end end
       Module type declarations do not match:
         module type S = sig module N = X.N @@ stateless end
       does not match
         module type S = sig module N = X.N end
       The second module type is not included in the first
       At position "module type S = <here>"
       Module types do not match:
         sig module N = X.N end
       is not equal to
         sig module N = X.N @@ stateless end
       At position "module type S = <here>"
       Modalities on N do not match:
       The second is portable and the first is not.
|}];;

module type S = sig
  module M : sig
    module A : sig end
    module B : sig end
  end
  module N = M.A
end

module Foo = struct
  module B = struct let x = 0 end
  module A = struct let x = "hello" end
end

module Bar : S with module M := Foo = struct module N = Foo.A end

let s : string = Bar.N.x
[%%expect {|
module type S =
  sig
    module M : sig module A : sig end module B : sig end end
    module N = M.A
  end
module Foo :
  sig module B : sig val x : int end module A : sig val x : string end end
module Bar : sig module N = Foo.A end
val s : string = "hello"
|}]


module M : sig
  module N : sig
    module A : sig val x : string end
    module B : sig val x : int end
  end
  module F (X : sig module A = N.A end) : sig val s : string end
end = struct
  module N = struct
    module B = struct let x = 0 end
    module A = struct let x = "hello" end
  end
  module F (X : sig module A : sig val x : string end end) = struct
    let s = X.A.x
  end
end

module N = M.F(struct module A = M.N.A end)

let s : string = N.s
[%%expect {|
module M :
  sig
    module N :
      sig
        module A : sig val x : string end
        module B : sig val x : int end
      end
    module F : functor (X : sig module A = N.A end) -> sig val s : string end
  end
module N : sig val s : string end
val s : string = "hello"
|}]
