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

type t8 : bits8 addressable

type tb8 : bits8
[%%expect{|
type t8 : bits8 addressable
type tb8 : bits8
|}]

module type Check = module type of struct
  module F (M : S @ static) = struct
    (* x := bits64; [bits64 addressable = bits64] *)
    let g1 (a : int64#) (b : int64#) = M.f a b

    (* x := value *)
    let g2 (a : string) (b : string) = M.f a b

    (* x := bits8 addressable; [(bits8 addressable) addressable] absorbs *)
    let g3 (a : t8) (b : t8) = M.f a b

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
          val g3 : t8 -> t8 -> unit
          val g4 : int64# -> unit
        end
      @@ stateless
  end
|}]

(* When the wrapped use is checked first, [x addressable = bits8 addressable]
   must not cancel the operator and commit [x] to [bits8]: here the bare use
   needs [x = bits8 addressable]. *)
module type S_swapped = sig
  val f : layout_ x. ('a : x) ('b : x addressable). 'b -> 'a -> unit
end
[%%expect{|
module type S_swapped =
  sig val f : layout_ l. ('b : l addressable) ('a : l). 'b -> 'a -> unit end
|}]

module type Check_wrapped_first = module type of struct
  module F (M : S_swapped @ static) = struct
    let g (a : t8) (b : t8) = M.f b a
  end
end
[%%expect{|
module type Check_wrapped_first =
  sig
    module F :
      functor (M : S_swapped @ static) -> sig val g : t8 -> t8 -> unit end @@
      stateless
  end
|}]

(* Incompleteness: a variable payload compared against a non-variable payload
   commits to the absorbing solution, so a later bare use at the unaddressable
   kind is rejected even though [x = bits8] was also a solution. *)
module type Check_bad_absorbed = module type of struct
  module F (M : S_swapped @ static) = struct
    let bad (a : tb8) (b : t8) = M.f b a
  end
end
[%%expect{|
Line 3, characters 39-40:
3 |     let bad (a : tb8) (b : t8) = M.f b a
                                           ^
Error: The value "a" has type "tb8" but an expression was expected of type
         "('a : bits8 addressable)"
       The layout of tb8 is bits8
         because of the definition of tb8 at line 3, characters 0-16.
       But the layout of tb8 must be a sublayout of bits8 addressable
         because of the definition of f at line 2, characters 2-68.
|}]

(* x := bits8, but the second argument must then be [bits8 addressable] *)
module type Check_bad = module type of struct
  module F (M : S @ static) = struct
    let bad (a : tb8) (b : tb8) = M.f a b
  end
end
[%%expect{|
Line 3, characters 40-41:
3 |     let bad (a : tb8) (b : tb8) = M.f a b
                                            ^
Error: The value "b" has type "tb8" but an expression was expected of type
         "('a : bits8 addressable)"
       The layout of tb8 is bits8
         because of the definition of tb8 at line 3, characters 0-16.
       But the layout of tb8 must be a sublayout of bits8 addressable
         because of the definition of f at line 2, characters 2-68.
|}]
