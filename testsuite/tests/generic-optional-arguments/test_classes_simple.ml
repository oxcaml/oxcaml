(* TEST
 flags = "-extension-universe alpha";
 expect;
*)

type 'a my_option =
  | MyNone
  | MySome of 'a
[@@option_like]

(* Test 1: Class with generic optional as constructor argument - SHOULD FAIL *)
class with_optional_arg (?x : int my_option) () = object
  method get = match x with MyNone -> 0 | MySome n -> n
end
[%%expect{|
type 'a my_option = MyNone | MySome of 'a
Line 7, characters 26-43:
7 | class with_optional_arg (?x : int my_option) () = object
                              ^^^^^^^^^^^^^^^^^
Error: Generic optional arguments are not supported in class arguments
|}]

(* Test 1: Class with generic optional as constructor argument - SHOULD FAIL *)
class with_optional_arg_2 (name: string) (?x : int my_option) () = object
  method get = match x with MyNone -> 0 | MySome n -> n
end
[%%expect{|
Line 1, characters 43-60:
1 | class with_optional_arg_2 (name: string) (?x : int my_option) () = object
                                               ^^^^^^^^^^^^^^^^^
Error: Generic optional arguments are not supported in class arguments
|}]

(* Test 1b: Class with generic optional as constructor argument - SHOULD FAIL *)
class with_vanilla_optional_arg_default ?(x : int = 2) () = object
  method get : int = x
end
[%%expect{|
class with_vanilla_optional_arg_default :
  ?x:int -> unit -> object method get : int end
|}]


(* Test 1b: Class with generic optional as constructor argument - SHOULD FAIL *)
class with_optional_arg_default (?(x = 2) : int my_option) () = object
  method get = x
end
[%%expect{|
Line 1, characters 34-57:
1 | class with_optional_arg_default (?(x = 2) : int my_option) () = object
                                      ^^^^^^^^^^^^^^^^^^^^^^^
Error: Generic optional arguments are not supported in class arguments
|}]

(* Test 3: Class with regular (non-optional) constructor argument - SHOULD WORK *)
class with_regular_arg (x : int) = object
  method get = x
  method process_optional (?y : int my_option) () =
    match y with MyNone -> x | MySome n -> n
end
[%%expect{|
class with_regular_arg :
  int ->
  object
    method get : int
    method process_optional : (?y):int my_option -> unit -> int
  end
|}]

(* Test 4: Class type with generic optional in method signature - SHOULD WORK *)
class type interface = object
  method m : (?x):int my_option -> unit -> int
end
[%%expect{|
class type interface =
  object method m : (?x):int my_option -> unit -> int end
|}]

(* Test usage *)

let obj3 = new with_optional_arg ()
let _ = obj3#get
let obj4 = new with_optional_arg ?x:MyNone ()
let _ = obj4#get
let obj5 = new with_optional_arg ~x:123 ()
let _ = obj5#get
[%%expect{|
Line 1, characters 15-32:
1 | let obj3 = new with_optional_arg ()
                   ^^^^^^^^^^^^^^^^^
Error: Unbound class "with_optional_arg"
|}]


let obj3 = new with_optional_arg_default ()
let _ = obj3#get
let obj4 = new with_optional_arg_default ?x:MyNone ()
let _ = obj4#get
let obj4 = new with_optional_arg_default ?x:(MySome 4) ()
let _ = obj4#get
let obj5 = new with_optional_arg_default ~x:123 ()
let _ = obj5#get
[%%expect{|
Line 1, characters 15-40:
1 | let obj3 = new with_optional_arg_default ()
                   ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unbound class "with_optional_arg_default"
|}]


class obj_class = with_optional_arg_2 "hello" ()
[%%expect{|
Line 1, characters 18-37:
1 | class obj_class = with_optional_arg_2 "hello" ()
                      ^^^^^^^^^^^^^^^^^^^
Error: Unbound class "with_optional_arg_2"
|}]

class obj_class = with_optional_arg_2 "hello" ~x:2 ()
[%%expect{|
Line 1, characters 18-37:
1 | class obj_class = with_optional_arg_2 "hello" ~x:2 ()
                      ^^^^^^^^^^^^^^^^^^^
Error: Unbound class "with_optional_arg_2"
|}]
