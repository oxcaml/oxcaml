(* TEST
 expect;
*)

(* Classes may refer to the classes of their recursive group from the parts
   of the class body that are only evaluated later. *)

class c = object method m = new c end;;
[%%expect{|
class c : object method m : c end
|}];;

class c = object method d = new d end
and d = object method c = new c end;;
[%%expect{|
class c : object method d : d end
and d : object method c : c end
|}];;

class c = object (self) method d = (new d)#c method c = self end
and d = object method c = new c end;;
[%%expect{|
class c : object ('a) method c : 'a method d : c end
and d : object method c : c end
|}];;

class c = let f () = () in object method m = f () end
and d = object method c = new c end;;
[%%expect{|
class c : object method m : unit end
and d : object method c : c end
|}];;

class c = object initializer ignore (new c) end;;
[%%expect{|
class c : object  end
|}];;

(* The let-bindings of a class are evaluated when the class is defined and
   may not refer to the recursive group, not even from a function. *)

class c = let f () = new c in object method m = f () end;;
[%%expect{|
Line 1, characters 10-56:
1 | class c = let f () = new c in object method m = f () end;;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of recursive class expression is not allowed.
       It refers to the class "c",
       which is defined in the same recursive group.
|}];;

class c = let o = object inherit d end in object method o = o end
and d = object end;;
[%%expect{|
Line 1, characters 33-34:
1 | class c = let o = object inherit d end in object method o = o end
                                     ^
Error: The class "d" is not yet completely defined
|}];;

class c = let g () = object inherit d end in object method o = g () end
and d = object end;;
[%%expect{|
Line 1, characters 36-37:
1 | class c = let g () = object inherit d end in object method o = g () end
                                        ^
Error: The class "d" is not yet completely defined
|}];;

(* Classes in [let rec] bindings. *)

class c = object end
let rec x = fun () -> new c;;
[%%expect{|
class c : object  end
val x : unit -> c = <fun>
|}];;

class c _ = object end
let rec x = new c x;;
[%%expect{|
class c : 'a -> object  end
Line 2, characters 12-19:
2 | let rec x = new c x;;
                ^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec".
       It refers to "x", but the names bound by a "let rec" may only be used
       by the functions and lazy values it binds or, through constructors,
       in the mutable fields of the records and references it binds.
|}];;

let rec x = y#m and y = object method m = () end;;
[%%expect{|
Line 1, characters 12-15:
1 | let rec x = y#m and y = object method m = () end;;
                ^^^
Error: This kind of expression is not allowed as right-hand side of "let rec".
       It refers to "y", but the names bound by a "let rec" may only be used
       by the functions and lazy values it binds or, through constructors,
       in the mutable fields of the records and references it binds.
|}];;
