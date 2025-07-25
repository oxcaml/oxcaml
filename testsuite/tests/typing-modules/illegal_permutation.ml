(* TEST
   expect;
*)
class type ct = object end

module type s = sig
  type a

  val one : int

  type b

  class two : ct

  type c

  type exn += Three

  type d
end

module type c12 = sig
  type a

  class two : ct

  type b

  val one : int

  type c

  type exn += Three

  type d
end

module type c123 = sig
  type a

  type exn += Three

  type b

  class two : ct

  type c

  val one : int

  type d
end

module type expected = sig
  module type x = s
end

module A : expected = struct
  module type x = c12
end

[%%expect
{|
class type ct = object  end
module type s =
  sig
    type a
    val one : int
    type b
    class two : ct
    type c
    type exn += Three
    type d
  end
module type c12 =
  sig
    type a
    class two : ct
    type b
    val one : int
    type c
    type exn += Three
    type d
  end
module type c123 =
  sig
    type a
    type exn += Three
    type b
    class two : ct
    type c
    val one : int
    type d
  end
module type expected = sig module type x = s end
Lines 55-57, characters 22-3:
55 | ......................struct
56 |   module type x = c12
57 | end
Error: Signature mismatch:
       Modules do not match:
         sig module type x = c12 end
       is not included in
         expected
       Module type declarations do not match:
         module type x = c12
       does not match
         module type x = s
       At position "module type x = <here>"
       Illegal permutation of runtime components in a module type.
         For example,
         the class "two" and the value "one" are not in the same order
         in the expected and actual module types.
|}]

module B : expected = struct
  module type x = c123
end

[%%expect
{|
Lines 1-3, characters 22-3:
1 | ......................struct
2 |   module type x = c123
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig module type x = c123 end
       is not included in
         expected
       Module type declarations do not match:
         module type x = c123
       does not match
         module type x = s
       At position "module type x = <here>"
       Illegal permutation of runtime components in a module type.
         For example,
         the exception "Three" and the value "one" are not in the same order
         in the expected and actual module types.
|}]

module Far : sig
  module type x = sig
    val a : int

    val b : int

    val c : int

    val d : int

    val e : int
  end
end = struct
  module type x = sig
    val a : int

    val b : int

    val e : int

    val d : int

    val c : int
  end
end

[%%expect
{|
Lines 13-25, characters 6-3:
13 | ......struct
14 |   module type x = sig
15 |     val a : int
16 |
17 |     val b : int
...
22 |
23 |     val c : int
24 |   end
25 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module type x =
             sig
               val a : int
               val b : int
               val e : int
               val d : int
               val c : int
             end
         end
       is not included in
         sig
           module type x =
             sig
               val a : int
               val b : int
               val c : int
               val d : int
               val e : int
             end
         end
       Module type declarations do not match:
         module type x =
           sig
             val a : int
             val b : int
             val e : int
             val d : int
             val c : int
           end
       does not match
         module type x =
           sig
             val a : int
             val b : int
             val c : int
             val d : int
             val e : int
           end
       At position "module type x = <here>"
       Illegal permutation of runtime components in a module type.
         For example,
         the value "e" and the value "c" are not in the same order
         in the expected and actual module types.
|}]

module Confusing : sig
  module type x = sig
    class x : ct

    val x : int
  end
end = struct
  module type x = sig
    val x : int

    class x : ct
  end
end

[%%expect
{|
Lines 7-13, characters 6-3:
 7 | ......struct
 8 |   module type x = sig
 9 |     val x : int
10 |
11 |     class x : ct
12 |   end
13 | end
Error: Signature mismatch:
       Modules do not match:
         sig module type x = sig val x : int class x : ct end end
       is not included in
         sig module type x = sig class x : ct val x : int end end
       Module type declarations do not match:
         module type x = sig val x : int class x : ct end
       does not match
         module type x = sig class x : ct val x : int end
       At position "module type x = <here>"
       Illegal permutation of runtime components in a module type.
         For example,
         the value "x" and the class "x" are not in the same order
         in the expected and actual module types.
|}]

module MT : sig
  module type a = sig
    module type b = sig
      val x : int

      val y : int
    end
  end
end = struct
  module type a = sig
    module type b = sig
      val y : int

      val x : int
    end
  end
end

[%%expect
{|
Lines 9-17, characters 6-3:
 9 | ......struct
10 |   module type a = sig
11 |     module type b = sig
12 |       val y : int
13 |
14 |       val x : int
15 |     end
16 |   end
17 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module type a =
             sig module type b = sig val y : int val x : int end end
         end
       is not included in
         sig
           module type a =
             sig module type b = sig val x : int val y : int end end
         end
       Module type declarations do not match:
         module type a =
           sig module type b = sig val y : int val x : int end end
       does not match
         module type a =
           sig module type b = sig val x : int val y : int end end
       At position "module type a = <here>"
       Module types do not match:
         sig module type b = sig val y : int val x : int end end
       is not equal to
         sig module type b = sig val x : int val y : int end end
       At position "module type a = <here>"
       Module type declarations do not match:
         module type b = sig val y : int val x : int end
       does not match
         module type b = sig val x : int val y : int end
       At position "module type a = sig module type b = <here> end"
       Illegal permutation of runtime components in a module type.
         For example,
         the value "y" and the value "x" are not in the same order
         in the expected and actual module types.
|}]

class type ct = object end

module Classes : sig
  module type x = sig
    class a : ct

    class b : ct
  end
end = struct
  module type x = sig
    class b : ct

    class a : ct
  end
end

[%%expect
{|
class type ct = object  end
Lines 9-15, characters 6-3:
 9 | ......struct
10 |   module type x = sig
11 |     class b : ct
12 |
13 |     class a : ct
14 |   end
15 | end
Error: Signature mismatch:
       Modules do not match:
         sig module type x = sig class b : ct class a : ct end end
       is not included in
         sig module type x = sig class a : ct class b : ct end end
       Module type declarations do not match:
         module type x = sig class b : ct class a : ct end
       does not match
         module type x = sig class a : ct class b : ct end
       At position "module type x = <here>"
       Illegal permutation of runtime components in a module type.
         For example,
         the class "b" and the class "a" are not in the same order
         in the expected and actual module types.
|}]

module Ext : sig
  module type x = sig
    type exn += A

    type exn += B
  end
end = struct
  module type x = sig
    type exn += B

    type exn += A
  end
end

[%%expect
{|
Lines 7-13, characters 6-3:
 7 | ......struct
 8 |   module type x = sig
 9 |     type exn += B
10 |
11 |     type exn += A
12 |   end
13 | end
Error: Signature mismatch:
       Modules do not match:
         sig module type x = sig type exn += B type exn += A end end
       is not included in
         sig module type x = sig type exn += A type exn += B end end
       Module type declarations do not match:
         module type x = sig type exn += B type exn += A end
       does not match
         module type x = sig type exn += A type exn += B end
       At position "module type x = <here>"
       Illegal permutation of runtime components in a module type.
         For example,
         the exception "B" and the exception "A" are not in the same order
         in the expected and actual module types.
|}]

module type w = sig
  module One : s

  module Two : s
end

module type w21 = sig
  module Two : s

  module One : s
end

module type wOne21 = sig
  module One : c12

  module Two : s
end

module C : sig
  module type x = w
end = struct
  module type x = w21
end

[%%expect
{|
module type w = sig module One : s module Two : s end
module type w21 = sig module Two : s module One : s end
module type wOne21 = sig module One : c12 module Two : s end
Lines 21-23, characters 6-3:
21 | ......struct
22 |   module type x = w21
23 | end
Error: Signature mismatch:
       Modules do not match:
         sig module type x = w21 end
       is not included in
         sig module type x = w end
       Module type declarations do not match:
         module type x = w21
       does not match
         module type x = w
       At position "module type x = <here>"
       Illegal permutation of runtime components in a module type.
         For example,
         the module "Two" and the module "One" are not in the same order
         in the expected and actual module types.
|}]

module D : sig
  module type x = w
end = struct
  module type x = wOne21
end

[%%expect
{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   module type x = wOne21
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig module type x = wOne21 end
       is not included in
         sig module type x = w end
       Module type declarations do not match:
         module type x = wOne21
       does not match
         module type x = w
       At position "module type x = <here>"
       Illegal permutation of runtime components in a module type.
         For example, in module "One",
         the class "two" and the value "one" are not in the same order
         in the expected and actual module types.
|}]

module F1 : sig
  module type x = functor (X : s) -> s
end = struct
  module type x = functor (X : c12) -> s
end

[%%expect
{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   module type x = functor (X : c12) -> s
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig module type x = functor (X : c12) -> s end
       is not included in
         sig module type x = functor (X : s) -> s end
       Module type declarations do not match:
         module type x = functor (X : c12) -> s
       does not match
         module type x = functor (X : s) -> s
       At position "module type x = <here>"
       Illegal permutation of runtime components in a module type.
         For example, at position "functor (X : <here>) -> ...",
         the class "two" and the value "one" are not in the same order
         in the expected and actual module types.
|}]

module F2 : sig
  module type x = functor (X : s) -> s
end = struct
  module type x = functor (X : s) -> c12
end

[%%expect
{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   module type x = functor (X : s) -> c12
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig module type x = functor (X : s) -> c12 end
       is not included in
         sig module type x = functor (X : s) -> s end
       Module type declarations do not match:
         module type x = functor (X : s) -> c12
       does not match
         module type x = functor (X : s) -> s
       At position "module type x = <here>"
       Illegal permutation of runtime components in a module type.
         For example, at position "functor (X) -> <here>",
         the class "two" and the value "one" are not in the same order
         in the expected and actual module types.
|}]

module Nested : sig
  module type x = sig
    module A : sig
      module B : sig
        module C : functor
          (X : sig end)
          (Y : sig end)
          (Z : sig
             module D : sig
               module E : sig
                 module F : functor
                   (X : sig end)
                   (Arg : sig
                      val one : int

                      val two : int
                    end)
                   -> sig end
               end
             end
           end)
          -> sig end
      end
    end
  end
end = struct
  module type x = sig
    module A : sig
      module B : sig
        module C : functor
          (X : sig end)
          (Y : sig end)
          (Z : sig
             module D : sig
               module E : sig
                 module F : functor
                   (X : sig end)
                   (Arg : sig
                      val two : int

                      val one : int
                    end)
                   -> sig end
               end
             end
           end)
          -> sig end
      end
    end
  end
end

[%%expect
{|
Lines 26-51, characters 6-3:
26 | ......struct
27 |   module type x = sig
28 |     module A : sig
29 |       module B : sig
30 |         module C : functor
...
48 |       end
49 |     end
50 |   end
51 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module type x =
             sig
               module A :
                 sig
                   module B :
                     sig
                       module C :
                         functor (X : sig end) (Y : sig end)
                           (Z : sig ... end) -> sig end
                     end
                 end
             end
         end
       is not included in
         sig
           module type x =
             sig
               module A :
                 sig
                   module B :
                     sig
                       module C :
                         functor (X : sig end) (Y : sig end)
                           (Z : sig ... end) -> sig end
                     end
                 end
             end
         end
       Module type declarations do not match:
         module type x =
           sig
             module A :
               sig
                 module B :
                   sig
                     module C :
                       functor (X : sig end) (Y : sig end)
                         (Z : sig module D : sig ... end end) -> sig end
                   end
               end
           end
       does not match
         module type x =
           sig
             module A :
               sig
                 module B :
                   sig
                     module C :
                       functor (X : sig end) (Y : sig end)
                         (Z : sig module D : sig ... end end) -> sig end
                   end
               end
           end
       At position "module type x = <here>"
       Illegal permutation of runtime components in a module type.
         For example,
         at position
           "module A :
             sig
               module B :
                 sig
                   module C(X)(Y)(Z :
                     sig
                       module D :
                         sig
                           module E : sig module F(X)(Arg : <here>) : ... end
                         end
                     end) : ...
                 end
             end",
         the value "two" and the value "one" are not in the same order
         in the expected and actual module types.
|}]
