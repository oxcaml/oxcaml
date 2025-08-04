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
class with_optional_arg :
  (?x):int my_option -> unit -> object method get : int end
|}]

(* Test 1: Class with generic optional as constructor argument - SHOULD FAIL *)
class with_optional_arg_2 (name: string) (?x : int my_option) () = object
  method get = match x with MyNone -> 0 | MySome n -> n
end
[%%expect{|
class with_optional_arg_2 :
  string -> (?x):int my_option -> unit -> object method get : int end
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
class with_optional_arg_default :
  (?x):int my_option -> unit -> object method get : int my_option end
|}]

(* Test 2: Class with NO constructor arguments, but method has generic optional - SHOULD WORK *)
class without_constructor_arg = object
  method process (x : int my_option) =
    match x with
    | MyNone -> 0
    | MySome n -> n
end
[%%expect{|
class without_constructor_arg :
  object method process : int my_option -> int end
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

(* Test usage of with_optional_arg *)
let obj3 = new with_optional_arg ()
let _ = obj3#get
let obj4 = new with_optional_arg ?x:MyNone ()
let _ = obj4#get
let obj5 = new with_optional_arg ~x:123 ()
let _ = obj5#get
[%%expect{|
val obj3 : with_optional_arg = <obj>
- : int = 0
val obj4 : with_optional_arg = <obj>
- : int = 0
val obj5 : with_optional_arg = <obj>
- : int = 123
|}]


(* Test usage of with_optional_arg_default *)
let obj3 = new with_optional_arg_default ()
let _ = obj3#get
let obj4 = new with_optional_arg_default ?x:MyNone ()
let _ = obj4#get
let obj4 = new with_optional_arg_default ?x:(MySome 4) ()
let _ = obj4#get
let obj5 = new with_optional_arg_default ~x:123 ()
let _ = obj5#get
[%%expect{|
val obj3 : with_optional_arg_default = <obj>
- : int my_option = MyNone
val obj4 : with_optional_arg_default = <obj>
- : int my_option = MyNone
val obj4 : with_optional_arg_default = <obj>
- : int my_option = MySome 4
val obj5 : with_optional_arg_default = <obj>
- : int my_option = MySome 123
|}]


let obj1 = new without_constructor_arg
let _ = obj1#process MyNone
let _ = obj1#process (MySome 42)

let obj2 = new with_regular_arg 100
let _ = obj2#process_optional ()
[%%expect{|
val obj1 : without_constructor_arg = <obj>
- : int = 0
- : int = 42
val obj2 : with_regular_arg = <obj>
- : int = 100
|}]


class obj_class = with_optional_arg_2 "hello" ()
[%%expect{|
Uncaught exception: Failure("TRIGGER GEN-OPT 2")

|}]

class obj_class = with_optional_arg_2 "hello" ~x:2 ()
[%%expect{|
Uncaught exception: Failure("TRIGGER GEN-OPT 1")

|}]
