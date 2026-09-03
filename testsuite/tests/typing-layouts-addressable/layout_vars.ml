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
    (* Check that [x addressable = x] when [x] is addressable, for different
       instantiations of the layout variable [x] *)
    let g1 (a : int64#) (b : int64#) = M.f a b

    let g2 (a : string) (b : string) = M.f a b

    let g3 (a : b8a) (b : b8a) = M.f a b

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

(* CR layouts: Inference for addressable is incomplete! These tests show that.
   See [Jkind.Sort.equate_sort_addressable].

   We should make these complete through "fixing the kind system." *)

(* [S_swapped] is [S] with the [x addressable] argument first. Checking that
   argument at [b8a] commits to [x = bits8]: the bare [x] argument then
   accepts [b8]... *)
module type S_swapped = sig
  val f : layout_ x. ('a : x addressable) ('b : x). 'a -> 'b -> unit
end

(* Unifies [bits8 addressable = x addressable] (which incompletely chooses to
   make [x = bits8]), then [bits8 = x] *)
module type Check_swapped = module type of struct
  module F (M : S_swapped @ static) = struct
    let g (a : b8a) (b : b8) = M.f a b
  end
end
[%%expect{|
module type S_swapped =
  sig val f : layout_ l. ('a : l addressable) ('b : l). 'a -> 'b -> unit end
module type Check_swapped =
  sig
    module F :
      functor (M : S_swapped @ static) -> sig val g : b8a -> b8 -> unit end
      @@ stateless
  end
|}]


(* Unifies [bits8 addressable = x addressable] (which incompletely chooses to
   make [x = bits8]), then attempts [bits8 addressable = x] *)
module type Check_bad_swapped = module type of struct
  module F (M : S_swapped @ static) = struct
    let bad (a : b8a) (b : b8a) = M.f a b
  end
end
[%%expect{|
Line 3, characters 40-41:
3 |     let bad (a : b8a) (b : b8a) = M.f a b
                                            ^
Error: The value "b" has type "b8a" but an expression was expected of type
         "('a : bits8)"
       The layout of b8a is bits8 addressable
         because of the definition of b8a at line 1, characters 0-28.
       But the layout of b8a must be a sublayout of bits8
         because of the definition of f at line 2, characters 2-68.
|}]

(* Unifies [bits8 addressable = x], then [bits8 addressable = x] *)
module type Check_unswapped = module type of struct
  module F (M : S @ static) = struct
    let g (a : b8a) (b : b8a) = M.f a b
  end
end
[%%expect{|
module type Check_unswapped =
  sig
    module F : functor (M : S @ static) -> sig val g : b8a -> b8a -> unit end
      @@ stateless
  end
|}]

(* Unifies [bits8 = x], then [bits8 = x addressable] *)
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

(** Inclusion checks with [addressable] **)

(* CR-soon jbachurski: Module coercion errors still show that [moregen]
   succeeded and will go away soon. *)

(* succeeds *)
module type Inclusion_narrowing = module type of struct
  module F (M : sig
    val g : layout_ x. ('a : x). 'a -> 'a
  end @ static) : sig
    val g : layout_ x. ('a : x addressable). 'a -> 'a
  end = M
end
[%%expect{|
Line 12, characters 8-9:
12 |   end = M
             ^
Error: Signature mismatch:
       Modules do not match:
         sig val poly_ g : 'a -> 'a end
       is not included in
         sig val g : layout_ l. ('a : l addressable). 'a -> 'a end
       Values do not match:
         val poly_ g : 'a -> 'a
       is not included in
         val g : layout_ l. ('a : l addressable). 'a -> 'a
       The layout parameter at position 1 in the first
       is instantiated with an unconstrained layout variable,
       which is not supported yet.
|}]

(* fails *)
module type Inclusion_widening = module type of struct
  module F (M : sig
    val g : layout_ x. ('a : x addressable). 'a -> 'a
  end @ static) : sig
    val g : layout_ x. ('a : x). 'a -> 'a
  end = M
end
[%%expect{|
Line 6, characters 8-9:
6 |   end = M
            ^
Error: Signature mismatch:
       Modules do not match:
         sig val g : layout_ l. ('a : l addressable). 'a -> 'a end
       is not included in
         sig val poly_ g : 'a -> 'a end
       Values do not match:
         val g : layout_ l. ('a : l addressable). 'a -> 'a
       is not included in
         val poly_ g : 'a -> 'a
       The type "'a -> 'a" is not compatible with the type "'b -> 'b"
       The kind of 'a is 's1 addressable
         because of the definition of g at line 5, characters 4-41.
       But the kind of 'a must be addressable
         because of the definition of g at line 3, characters 4-53.
|}]

(* incomplete -- commits to [x = bits8] instead of [x = bits8 addressable] *)
module type Inclusion_incomplete = module type of struct
  module F (M : sig
    val g : layout_ x. ('a : x addressable) ('b : x). 'a -> 'b
  end @ static) : sig
    val g : ('a : bits8 addressable) ('b : bits8 addressable). 'a -> 'b
  end = M
end
[%%expect{|
Line 6, characters 8-9:
6 |   end = M
            ^
Error: Signature mismatch:
       Modules do not match:
         sig val g : layout_ l. ('a : l addressable) ('b : l). 'a -> 'b end
       is not included in
         sig
           val g :
             ('a : bits8 addressable) ('b : bits8 addressable). 'a -> 'b
         end
       Values do not match:
         val g : layout_ l. ('a : l addressable) ('b : l). 'a -> 'b
       is not included in
         val g : ('a : bits8 addressable) ('b : bits8 addressable). 'a -> 'b
       The type "'a -> 'b" is not compatible with the type "'a -> 'c"
       The layout of 'a is bits8 addressable
         because of the definition of g at line 5, characters 4-71.
       But the layout of 'a must be a sublayout of bits8
         because of the definition of g at line 3, characters 4-62.
|}]

(* fine -- we choose [x = bits8] anyway *)
module type Inclusion_incomplete_but_ok = module type of struct
  module F (M : sig
    val g : layout_ x. ('a : x addressable) ('b : x). 'a -> 'b
  end @ static) : sig
    val g : ('a : bits8 addressable) ('b : bits8). 'a -> 'b
  end = M
end
[%%expect{|
Line 6, characters 8-9:
6 |   end = M
            ^
Error: Signature mismatch:
       Modules do not match:
         sig val g : layout_ l. ('a : l addressable) ('b : l). 'a -> 'b end
       is not included in
         sig val g : ('a : bits8 addressable) ('b : bits8). 'a -> 'b end
       Values do not match:
         val g : layout_ l. ('a : l addressable) ('b : l). 'a -> 'b
       is not included in
         val g : ('a : bits8 addressable) ('b : bits8). 'a -> 'b
       the first has 1 more layout parameter that is not used,
       which is not supported yet.
|}]

(* fine -- correct in this order, since we set [x = bits8 addressable] first *)
module type Inclusion_incomplete_but_swapped_ok = module type of struct
  module F (M : sig
    val g : layout_ x. ('a : x) ('b : x addressable). 'a -> 'b
  end @ static) : sig
    val g : ('a : bits8 addressable) ('b : bits8 addressable). 'a -> 'b
  end = M
end
[%%expect{|
Line 6, characters 8-9:
6 |   end = M
            ^
Error: Signature mismatch:
       Modules do not match:
         sig val g : layout_ l. ('a : l) ('b : l addressable). 'a -> 'b end
       is not included in
         sig
           val g :
             ('a : bits8 addressable) ('b : bits8 addressable). 'a -> 'b
         end
       Values do not match:
         val g : layout_ l. ('a : l) ('b : l addressable). 'a -> 'b
       is not included in
         val g : ('a : bits8 addressable) ('b : bits8 addressable). 'a -> 'b
       the first has 1 more layout parameter that is not used,
       which is not supported yet.
|}]

