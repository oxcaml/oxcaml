(* TEST
flags = "-extension-universe alpha -dlambda -dno-unique-ids";

 expect;
*)

let f_option (?(x = 33) : int option) () = x
[%%expect{|
(let
  (f_option =
     (function {nlocal = 0} *option*x[(consts (0)) (non_consts ([0: *]))]
       param[int] : int (if *option*x (field_imm 0 *option*x) 33)))
  (apply (field_imm 1 (global Toploop!)) "f_option" f_option))
val f_option : (?x):int option -> unit -> int = <fun>
|}]

let f_option_no_default (?x : int option) () = x
[%%expect{|
(let
  (f_option_no_default =
     (function {nlocal = 0} x[(consts (0)) (non_consts ([0: *]))] param[int]
       [(consts (0)) (non_consts ([0: *]))]x))
  (apply (field_imm 1 (global Toploop!)) "f_option_no_default"
    f_option_no_default))
val f_option_no_default : (?x):int option -> unit -> int option = <fun>
|}]

let f_or_null (?(x = 48) : int or_null) () = x
[%%expect{|
(let
  (f_or_null =
     (function {nlocal = 0} *or_null*x param[int] : int
       (if (isnull *or_null*x) 48 *or_null*x)))
  (apply (field_imm 1 (global Toploop!)) "f_or_null" f_or_null))
val f_or_null : (?x):int or_null -> unit -> int = <fun>
|}]
let f_or_null_no_default (?x : int or_null) () = x

[%%expect{|
(let (f_or_null_no_default = (function {nlocal = 0} x param[int] x))
  (apply (field_imm 1 (global Toploop!)) "f_or_null_no_default"
    f_or_null_no_default))
val f_or_null_no_default : (?x):int or_null -> unit -> int or_null = <fun>
|}]

(* Test calling them *)

let _ = f_option ()
[%%expect{|
(let (f_option = (apply (field_imm 0 (global Toploop!)) "f_option"))
  (apply f_option 0 0))
- : int = 33
|}]

let _ = f_option ~x:21 ()
[%%expect{|
(let (f_option = (apply (field_imm 0 (global Toploop!)) "f_option"))
  (apply f_option [0: 21] 0))
- : int = 21
|}]

let _ = f_option_no_default ()
[%%expect{|
(let
  (f_option_no_default =
     (apply (field_imm 0 (global Toploop!)) "f_option_no_default"))
  (apply f_option_no_default 0 0))
- : int option = None
|}]

let _ = f_option_no_default ~x:21 ()
[%%expect{|
(let
  (f_option_no_default =
     (apply (field_imm 0 (global Toploop!)) "f_option_no_default"))
  (apply f_option_no_default [0: 21] 0))
- : int option = Some 21
|}]

let _ = f_or_null ()
[%%expect{|
(let (f_or_null = (apply (field_imm 0 (global Toploop!)) "f_or_null"))
  (apply f_or_null <null> 0))
- : int = 48
|}]

let _ = f_or_null ~x:21 ()
[%%expect{|
(let (f_or_null = (apply (field_imm 0 (global Toploop!)) "f_or_null"))
  (apply f_or_null 21 0))
- : int = 21
|}]


let _ = f_or_null_no_default ()
[%%expect{|
(let
  (f_or_null_no_default =
     (apply (field_imm 0 (global Toploop!)) "f_or_null_no_default"))
  (apply f_or_null_no_default <null> 0))
- : int or_null = Null
|}]

let _ = f_or_null_no_default ~x:21 ()
[%%expect{|
(let
  (f_or_null_no_default =
     (apply (field_imm 0 (global Toploop!)) "f_or_null_no_default"))
  (apply f_or_null_no_default 21 0))
- : int or_null = This 21
|}]
