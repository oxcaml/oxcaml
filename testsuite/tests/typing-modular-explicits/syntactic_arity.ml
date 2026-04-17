(* TEST
 flags = "-extension modular_explicits";
 expect;
*)

(* Interaction of the syntactic-arity check with module-dependent
   functions, from typing-gadts/syntactic-arity.ml (ocaml/ocaml#14721),
   which runs without the modular_explicits extension. *)

type (_, _) eq = Eq : ('a, 'a) eq

[%%expect{|
type (_, _) eq = Eq : ('a, 'a) eq
|}]

module type t = sig type _ t end
let f (type a) (module X: t) (Equal : (a X.t, int) Type.eq) : string = ""
 [%%expect {|
module type t = sig type _ t end
val f : (module X : t) -> ('a X.t, int) Type.eq -> string = <fun>
|}]

module type T = sig
  type t
  val x: t
  type f = int -> int
 end

let ok (type a) (module M:T) ?opt:((Eq : (a, M.f) eq) = assert false) () : a =
  function x -> x + 1;;
[%%expect {|
module type T = sig type t val x : t type f = int -> int end
val ok :
  ('a : any) ('b : any).
    (module M : T) -> ?opt:('a -> 'b, M.f) eq -> unit -> 'a -> 'b =
  <fun>
|}]

let fail: type a. (module M:T) -> (a,M.f) eq -> a =
  fun (module M) Eq x -> 1 + x
[%%expect{|
Line 2, characters 2-30:
2 |   fun (module M) Eq x -> 1 + x
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The syntactic arity of the function doesn't match the type constraint:
       This function has 3 syntactic arguments, but its type is constrained to
         "(module M : T) -> (a, M.f) eq -> a".
        Hint: consider splitting the function definition into
          "fun ... gadt_pat -> fun ..."
          where "gadt_pat" is the pattern with the GADT constructor that
          introduces the local type equation on "a".
|}]

let ok: (module M:T) -> M.f =
  fun (module M) x -> 1 + x

[%%expect{|
val ok : (module M : T) -> M.f = <fun>
|}]
