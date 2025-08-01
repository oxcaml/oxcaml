(* TEST
 flags = "-extension-universe alpha";
 expect;
*)

type 'a my_option =
  | MyNone
  | MySome of 'a
[@@option_like]

(* Test 1: Class with method having generic optional argument - should work *)
class test_method_only = object
  method process (?x : int my_option) () =
    match x with
    | MyNone -> 0
    | MySome n -> n
end
[%%expect{|
type 'a my_option = MyNone | MySome of 'a
class test_method_only :
  object method process : (?x):int my_option -> unit -> int end
|}]

(* Test 2: Use the class with method *)
let obj1 = new test_method_only
let v1 = obj1#process ()
let v2 = obj1#process ?x:(MySome 42) ()
[%%expect{|
val obj1 : test_method_only = <obj>
val v1 : int = 0
val v2 : int = 42
|}]

(* Test 3: Class with generic optional constructor argument - should fail *)
class test_constructor_arg (?x : int my_option) = object
  method get = match x with MyNone -> 0 | MySome n -> n
end
[%%expect{|
Exception: Failure "Cannot create fresh generic optional without existing type".
|}]

(* Test 4: Class type with generic optional in method signature *)
class type interface = object
  method m : (?x):int my_option -> unit -> int
end
[%%expect{|
class type interface =
  object method m : (?x):int my_option -> unit -> int end
|}]

(* Test 5: Class implementing the interface *)
class implementation : interface = object
  method m (?x : int my_option) () =
    match x with MyNone -> 0 | MySome n -> n
end
[%%expect{|
class implementation : interface
|}]

(* Test 5a: Use implementation class *)
let obj_impl = new implementation
let v_impl1 = obj_impl#m ()
let v_impl2 = obj_impl#m ?x:(MySome 99) ()
[%%expect{|
val obj_impl : implementation = <obj>
val v_impl1 : int = 0
val v_impl2 : int = 99
|}]

(* Test 6: Class with both regular and generic optional arguments *)
class mixed_args (regular : int) = object
  method use_regular = regular
  method use_generic (?opt : string my_option) () =
    match opt with
    | MyNone -> "none"
    | MySome s -> s
end
[%%expect{|
class mixed_args :
  int ->
  object
    method use_generic : (?opt):string my_option -> unit -> string
    method use_regular : int
  end
|}]

(* Test 6a: Use mixed args class *)
let obj_mixed = new mixed_args 42
let v_reg = obj_mixed#use_regular
let v_gen1 = obj_mixed#use_generic ()
let v_gen2 = obj_mixed#use_generic ~opt:(MySome "test") ()
[%%expect{|
val obj_mixed : mixed_args = <obj>
val v_reg : int = 42
val v_gen1 : string = "none"
val v_gen2 : string = "test"
|}]

(* Test 7: Inheritance with generic optional methods *)
class base = object
  method virtual process : (?x) : int my_option -> unit -> int
end
[%%expect{|
class base : object method virtual process : (?x : int my_option) -> unit -> int end
|}]

class derived = object
  inherit base
  method process (?x : int my_option) () =
    match x with MyNone -> -1 | MySome n -> n
end
[%%expect{|
class derived : object method process : (?x : int my_option) -> unit -> int end
|}]

(* Test 7a: Use the derived class *)
let obj_derived = new derived
let v3 = obj_derived#process ()
let v4 = obj_derived#process ?x:(MySome 100) ()
[%%expect{|
val obj_derived : derived = <obj>
val v3 : int = -1
val v4 : int = 100
|}]

(* Test 8: Multiple generic optional arguments in method *)
class multiple_optionals = object
  method multi (?x : int my_option) (?y : string my_option) () =
    let x_val = match x with MyNone -> 0 | MySome n -> n in
    let y_val = match y with MyNone -> "" | MySome s -> s in
    (x_val, y_val)
end
[%%expect{|
class multiple_optionals :
  object
    method multi :
      (?x):int my_option -> (?y):string my_option -> unit -> int * string
  end
|}]

(* Test 8a: Use multiple optionals *)
let obj_multi = new multiple_optionals
let v5 = obj_multi#multi ()
let v6 = obj_multi#multi ?x:(MySome 5) ()
let v7 = obj_multi#multi ~y:(MySome "hello") ()
let v8 = obj_multi#multi ?x:(MySome 10) ~y:(MySome "world") ()
[%%expect{|
val obj_multi : multiple_optionals = <obj>
val v5 : int * string = (0, "")
val v6 : int * string = (5, "")
val v7 : int * string = (0, "hello")
val v8 : int * string = (10, "world")
|}]

(* Test 9: Class with mutable field and generic optional method *)
class with_state = object
  val mutable count = 0
  method increment (?by : int my_option) () =
    let inc = match by with MyNone -> 1 | MySome n -> n in
    count <- count + inc;
    count
end
[%%expect{|
class with_state :
  object
    val mutable count : int
    method increment : (?by):int my_option -> unit -> int
  end
|}]

(* Test 9a: Use stateful class *)
let obj_state = new with_state
let v9 = obj_state#increment ()
let v10 = obj_state#increment ~by:(MySome 5) ()
let v11 = obj_state#increment ()
[%%expect{|
val obj_state : with_state = <obj>
val v9 : int = 1
val v10 : int = 6
val v11 : int = 7
|}]

(* Test 10: Using generic optional with polymorphic method *)
class poly_optional = object
  method poly : 'a. (?x) : 'a my_option -> unit -> 'a option =
    fun ?(x : 'a my_option) () ->
      match x with
      | MyNone -> None
      | MySome v -> Some v
end
[%%expect{|
class poly_optional :
  object method poly : (?x : 'a my_option) -> unit -> 'a option end
|}]

(* Test 10a: Use polymorphic optional *)
let obj_poly = new poly_optional
let v12 = obj_poly#poly ()
let v13 = obj_poly#poly ?x:(MySome 42) ()
let v14 = obj_poly#poly ?x:(MySome "poly") ()
[%%expect{|
val obj_poly : poly_optional = <obj>
val v12 : '_weak1 option = None
val v13 : int option = Some 42
val v14 : string option = Some "poly"
|}]
