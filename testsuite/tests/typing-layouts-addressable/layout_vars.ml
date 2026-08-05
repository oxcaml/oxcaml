(* TEST
 flags = "-extension layouts_alpha -extension layout_poly_alpha";
 expect;
*)

(* Tests for the [addressable] kind operator applied to layout variables.
   Translation of layout-polymorphic instantiations is not supported yet, so
   the uses are wrapped in [module type of] to only typecheck them. *)

module type S = sig
  val f : layout_ x. ('a : x) ('b : x addressable). 'a -> 'b -> unit
end
[%%expect{|
module type S =
  sig val f : layout_ l. ('a : l) ('b : l addressable). 'a -> 'b -> unit end
|}]

type b8a : bits8 addressable

type b8 : bits8
[%%expect{|
type b8a : bits8 addressable
type b8 : bits8
|}]

module type Check = module type of struct
  module F (M : S @ static) = struct
    (* x := bits64; [bits64 addressable = bits64] *)
    let g1 (a : int64#) (b : int64#) = M.f a b

    (* x := value *)
    let g2 (a : string) (b : string) = M.f a b

    (* x := bits8 addressable; [(bits8 addressable) addressable] absorbs *)
    let g3 (a : b8a) (b : b8a) = M.f a b

    (* x and x addressable at the same argument: constrains x to be
       addressable *)
    let g4 (a : int64#) = M.f a a
  end
end
[%%expect{|
module type Check =
  sig
    module F :
      functor (M : S @ static) ->
        sig
          val g1 : int64# -> int64# -> unit
          val g2 : string -> string -> unit
          val g3 : b8a -> b8a -> unit
          val g4 : int64# -> unit
        end
      @@ stateless
  end
|}]

(* [x addressable = bits8 addressable] admits both [x = bits8] and
   [x = bits8 addressable], and no sort represents both; when the wrapped use
   is checked first, unification commits to the cancelling solution
   [x = bits8]. *)
module type S_swapped = sig
  val f : layout_ x. ('a : x) ('b : x addressable). 'b -> 'a -> unit
end
[%%expect{|
module type S_swapped =
  sig val f : layout_ l. ('b : l addressable) ('a : l). 'b -> 'a -> unit end
|}]

(* The committed solution: the bare use at the cancelled kind works... *)
module type Check_cancelled = module type of struct
  module F (M : S_swapped @ static) = struct
    let g (a : b8) (b : b8a) = M.f b a
  end
end
[%%expect{|
module type Check_cancelled =
  sig
    module F :
      functor (M : S_swapped @ static) -> sig val g : b8 -> b8a -> unit end
      @@ stateless
  end
|}]

(* ...but a later bare use at the addressable kind is rejected, even though
   [x = bits8 addressable] was also a solution (checking the bare use first,
   as in [g3] above, accepts). *)
module type Check_bad_cancelled = module type of struct
  module F (M : S_swapped @ static) = struct
    let bad (a : b8a) (b : b8a) = M.f b a
  end
end
[%%expect{|
Line 3, characters 40-41:
3 |     let bad (a : b8a) (b : b8a) = M.f b a
                                            ^
Error: The value "a" has type "b8a" but an expression was expected of type
         "('a : bits8)"
       The layout of b8a is bits8 addressable
         because of the definition of b8a at line 1, characters 0-28.
       But the layout of b8a must be a sublayout of bits8
         because of the definition of f at line 2, characters 2-68.
|}]

(* x := bits8, but the second argument must then be [bits8 addressable] *)
module type Check_bad = module type of struct
  module F (M : S @ static) = struct
    let bad (a : b8) (b : b8) = M.f a b
  end
end
[%%expect{|
Line 3, characters 38-39:
3 |     let bad (a : b8) (b : b8) = M.f a b
                                          ^
Error: The value "b" has type "b8" but an expression was expected of type
         "('a : bits8 addressable)"
       The layout of b8 is bits8
         because of the definition of b8 at line 3, characters 0-15.
       But the layout of b8 must be a sublayout of bits8 addressable
         because of the definition of f at line 2, characters 2-68.
|}]
