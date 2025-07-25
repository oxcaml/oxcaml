(* TEST
   expect;
*)

class type virtual ['a] c =
  object
    constraint 'a = [< `A of int & float]
  end

[%%expect
{|
Lines 1-4, characters 0-5:
1 | class type virtual ['a] c =
2 |   object
3 |     constraint 'a = [< `A of int & float]
4 |   end
Error: The type of this class,
       "class virtual ['_a] c :
         object constraint '_a = [< `A of int & float ] as '_weak1 end",
       contains non-collapsible conjunctive types in constraints.
       Type "int" is not compatible with type "float"
|}]

class type ct =
  object
    method x : int
  end

class c (y : 'a * float) : ct =
  object
    method x = y
  end

[%%expect
{|
class type ct = object method x : int end
Lines 7-9, characters 2-5:
7 | ..object
8 |     method x = y
9 |   end
Error: The class type object method x : 'a * float end
       is not matched by the class type ct
       The class type object method x : 'a * float end
       is not matched by the class type object method x : int end
       The method x has type "'a * float" but is expected to have type "int"
       Type "'a * float" is not compatible with type "int"
|}]

let foo = 42#m

[%%expect
{|
Line 1, characters 10-12:
1 | let foo = 42#m
              ^^
Error: This expression is not an object; it has type "int"
|}]

let foo =
  object (self)
    method foo = self#bar
  end

[%%expect
{|
Line 3, characters 17-21:
3 |     method foo = self#bar
                     ^^^^
Error: This expression has no method "bar"
|}]

class empty = object end

class also_empty =
  object
    inherit! empty
  end

[%%expect
{|
class empty : object  end
Line 5, characters 4-18:
5 |     inherit! empty
        ^^^^^^^^^^^^^^
Error: This inheritance does not override any methods or instance variables
       but is explicitly marked as overriding with "!".
|}]

class ['a] c =
  object
    val x : 'a list ref = ref []
  end

class ['a] x =
  let r = ref [] in
  object
    val x : 'a list ref = r
  end

[%%expect
{|
class ['a] c : object val x : 'a list ref end
Lines 6-10, characters 0-5:
 6 | class ['a] x =
 7 |   let r = ref [] in
 8 |   object
 9 |     val x : 'a list ref = r
10 |   end
Error: The type of this class,
       "class ['_a] x : object val x : '_a list ref end",
       contains the non-generalizable type variable(s): "'_a".
       (see manual section 6.1.2)
|}]
