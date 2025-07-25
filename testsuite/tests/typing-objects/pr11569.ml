(* TEST
   expect;
*)

class ['a] c =
  object
    constraint 'a = int

    method m (x : bool #c) = ()
  end

[%%expect
{|
Lines 1-6, characters 0-5:
1 | class ['a] c =
2 |   object
3 |     constraint 'a = int
4 |
5 |     method m (x : bool #c) = ()
6 |   end
Error: The class type "#c" is used with parameter(s) "bool ",
       whereas the class type definition constrains those parameters to be
       "int "
|}]

class ['a, 'b] c =
  object
    constraint 'a = int

    method m (x : (bool, 'b) #c) = ()
  end

[%%expect
{|
Lines 1-6, characters 0-5:
1 | class ['a, 'b] c =
2 |   object
3 |     constraint 'a = int
4 |
5 |     method m (x : (bool, 'b) #c) = ()
6 |   end
Error: The class type "#c" is used with parameter(s) "(bool, 'b) ",
       whereas the class type definition constrains those parameters to be
       "(int, 'b) "
|}]

class c =
  object
    method m (x : #c) = int_of_string x#m
  end

[%%expect
{|
Lines 1-4, characters 0-5:
1 | class c =
2 |   object
3 |     method m (x : #c) = int_of_string x#m
4 |   end
Error: The abbreviation "#c" expands to type "< m : 'a -> int; .. >"
       but is used with type "< m : string; .. > as 'a"
|}]
