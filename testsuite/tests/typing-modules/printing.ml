(* TEST
 expect;
*)

(* PR#6650 *)

module type S = sig
  class type c = object method m : int end
  module M : sig
    class type d = c
  end
end;;
module F (X : S) = X.M;;
[%%expect{|
module type S =
  sig
    class type c = object method m : int end
    module M : sig class type d = c end
  end
module F : (X : S) -> sig class type d = X.c end
|}];;

(* PR#6648 *)

module M = struct module N = struct let x = 1 end end;;
#show_module M;;
[%%expect{|
module M : sig module N : sig val x : int end end
module M : sig module N : sig ... end end
|}];;

(* Shortcut notation for functors *)
module type A
module type B
module type C
module type D
module type E
module type F
module Test(X: ((A->(B->C)->D) -> (E -> F))) = struct end
[%%expect {|
module type A
module type B
module type C
module type D
module type E
module type F
module Test : (X : (A -> (B -> C) -> D) -> E -> F) -> sig end
|}]

(* test reprinting of functors *)
module type LongFunctor1 = functor (X : A) () (_ : B) () -> C -> D -> sig end
[%%expect {|
<<<<<<< HEAD
module type LongFunctor1 =
  functor (X : A) () -> B -> functor () -> C -> D -> sig end
||||||| 23e84b8c4d
module type LongFunctor1 = functor (X : A) () (_ : B) () -> C -> D -> sig end
=======
module type LongFunctor1 = (X : A) () (_ : B) () -> C -> D -> sig end
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
|}]
module type LongFunctor2 = functor (_ : A) () (_ : B) () -> C -> D -> sig end
[%%expect {|
<<<<<<< HEAD
module type LongFunctor2 =
  A -> functor () -> B -> functor () -> C -> D -> sig end
||||||| 23e84b8c4d
module type LongFunctor2 = A -> functor () (_ : B) () -> C -> D -> sig end
=======
module type LongFunctor2 = A -> () (_ : B) () -> C -> D -> sig end
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
|}]
