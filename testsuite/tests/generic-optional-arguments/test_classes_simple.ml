(* TEST
 flags = "-extension-universe alpha";
 expect;
*)

type 'a my_option =
  | MyNone
  | MySome of 'a
[@@option_like]

(* Test 1: Class with generic optional as constructor argument - SHOULD FAIL *)
class with_optional_arg (?x : int my_option) = object
     method get = match x with MyNone -> 0 | MySome n -> n
   end
[%%expect{| |}]

(* Test 2: Class with NO constructor arguments, but method has generic optional - SHOULD WORK *)
class without_constructor_arg = object
  method process (x : int my_option) =
    match x with
    | MyNone -> 0
    | MySome n -> n
end
[%%expect{| |}]

(* Test 3: Class with regular (non-optional) constructor argument - SHOULD WORK *)
class with_regular_arg (x : int) = object
  method get = x
  method process_optional (?y : int my_option) () =
    match y with MyNone -> x | MySome n -> n
end
[%%expect{| |}]

(* Test 4: Class type with generic optional in method signature - SHOULD WORK *)
class type interface = object
  method m : (?x):int my_option -> unit -> int
end
[%%expect{| |}]

(* Test usage *)
let () =
  let obj1 = new without_constructor_arg in
  let _ = obj1#process MyNone in
  let _ = obj1#process (MySome 42) in

  let obj2 = new with_regular_arg 100 in
  let _ = obj2#process_optional () in
  ()
[%%expect{| |}]
