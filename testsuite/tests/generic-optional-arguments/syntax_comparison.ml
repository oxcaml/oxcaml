(* TEST
 flags = "-extension-universe alpha";

 expect;
*)


(**
Optional Syntax Rules
---------------------
	∣	 ? label-name
 	∣	 ? ( label-name [: typexpr] [= expr] )
 	∣	 ? label-name : pattern {zhichen: Actually here var only}
 	∣	 ? label-name : ( pattern [: typexpr] [= expr] )

Generic Optional Syntax Rules
-----------------------------
	∣	 <N/A>
 	∣	 (?  label-name [= expr] [: typexpr]  )
 	∣	 <N/A>
 	∣	 (? label-name :  pattern [= expr] [: typexpr]  )



Id | Optional Syntax               | Generic Optional Syntax       | Comment
---+-------------------------------+-------------------------------+----------
1  | ?x                            | <None>                        |
---+-------------------------------+-------------------------------+----------
2  | ?(x)                          | (?x)                          | Disallow
3  | ?(x = 1)                      | (?x = 1)                      | Disallow
4  | ?(x : int option)             | (?x : int option)             | [OK]
5  | ?(x : int = 1)                | (?(x = 1) : int option)       |
---+-------------------------------+-------------------------------+----------
6  | ?x:y                          | <None>                        |
---+-------------------------------+-------------------------------+----------
7  | ?x:(y)                        | (?x:y)                        | Disallow
8  | ?x:(y = 1)                    | (?x:y = 1)                    | Disallow
9  | ?x:(y:int option)             | (?x:y:int option)             | [OK]
10 | ?x:(y:int = 1)                | (?x:(y = 1): int option)      |
---+-------------------------------+-------------------------------+----------
11 | ?x:y:int [NOT OKAY]           | <None>                        |
---+-------------------------------+-------------------------------+----------
12 | ?x:(y:int option)             | (?x:(y:int option))           | Allowed!!
13 | ?x:(y:int = 1)                | (?x:(y = 1):int)              | Disallow
14 | ?x:((y:int option):int option)| (?x:(y:int option):int option)|
15 | ?x:((y:int):int = 1)          | (?x:((y:int) = 1): int option)|

! Not only are we switching ( with ?, we also switch [= expr] with [: typexpr].
*)

module type S = sig
  val f : ?x:int -> unit -> int
end

module type T = sig
  include S
  val g : (?x): int option -> unit -> int
  val h : (?x): int or_null -> unit -> int
end

let option_get x = match x with Some x -> x | None -> 12
let or_null_get x = match x with This x -> x | Null -> 12

[%%expect {|
module type S = sig val f : ?x:int -> unit -> int end
module type T =
  sig
    val f : ?x:int -> unit -> int
    val g : (?x):int option -> unit -> int
    val h : (?x):int or_null -> unit -> int
  end
val option_get : int option -> int = <fun>
val or_null_get : int or_null -> int = <fun>
|}]

(* Rule 1: ? label-name *)
module M_1 : S = struct
  let f ?x () = option_get x
end
[%%expect {|
module M_1 : S
|}]

(* No M_1 : T *)

(* Rule 2: ? ( label-name [: typexpr] [= expr] ) *)
module M_2 : S = struct
  let f ?(x) () = option_get x
end
[%%expect {|
module M_2 : S
|}]

module M_2_error : T = struct
  let f ?(x) () = option_get x
  let g (?x) () = option_get x
  let h (?x) () = option_get x
end
[%%expect {|
Line 3, characters 10-11:
3 |   let g (?x) () = option_get x
              ^
Error: Unknown generic optional argument type
|}]

module M_3 : S = struct
  let f ?(x = 5) () = x
end
[%%expect {|
module M_3 : S @@ stateless
|}]

module M_3_error : T = struct
  let f ?(x = 5) () = x
  let g (?(x = 5)) () = x
  let h (?(x = 5)) () = x
end
[%%expect {|
Line 3, characters 11-12:
3 |   let g (?(x = 5)) () = x
               ^
Error: Unknown generic optional argument type
|}]

module M_4 : T = struct
  let f ?(x : int option) () = option_get x
  let g (?x : int option) () = option_get x
  let h (?x : int or_null) () = or_null_get x
end
[%%expect {|
module M_4 : T
|}]

module M_5 : T = struct
  let f ?(x : int = 7) () = x
  let g (?(x = 7) : int option) () = x
  let h (?(x = 7) : int or_null) () = x
end
[%%expect {|
module M_5 : T @@ stateless
|}]

(* Rule 3: ? label-name : pattern *)
module M_6 : S = struct
  let f ?x:y () = option_get y
end
[%%expect {|
module M_6 : S
|}]

(* NO M_6 : T *)

(* Rule 4: ? label-name : ( pattern [: typexpr] [= expr] )*)
module M_7 : S = struct
  let f ?x:(y) () = option_get y
end
[%%expect {|
module M_7 : S
|}]

module M_7_error : T = struct
  let f ?x:(y) () = option_get y
  let g (?x:y) () = option_get y
  let h (?x:y) () = or_null_get y
end
[%%expect {|
Line 3, characters 12-13:
3 |   let g (?x:y) () = option_get y
                ^
Error: Unknown generic optional argument type
|}]

module M_8 : S = struct
  let f ?x:(y = 9) () = y
end
[%%expect {|
module M_8 : S @@ stateless
|}]

module M_8_error : T = struct
  let f ?x:(y = 9) () = y
  let g (?x:(y = 9)) () = y
  let h (?x:(y = 9)) () = y
end
[%%expect {|
Line 3, characters 13-14:
3 |   let g (?x:(y = 9)) () = y
                 ^
Error: Unknown generic optional argument type
|}]

module M_9 : T = struct
  let f ?x:(y : int option) () = option_get y
  let g (?x:y : int option) () = option_get y
  let h (?x:y : int or_null) () = or_null_get y
end
[%%expect {|
module M_9 : T
|}]

module M_10 : T = struct
  let f ?x:(y : int = 11) () = y
  let g (?x:(y = 11) : int option) () = y
  let h (?x:(y = 11) : int or_null) () = y
end
[%%expect {|
module M_10 : T @@ stateless
|}]

module M_12 : T = struct
  let f ?x:(y : int option) () = option_get y
  let g (?x:(y : int option)) () = option_get y
  let h (?x:(y : int or_null)) () = or_null_get y
end
[%%expect {|
module M_12 : T
|}]

module M_13 : S = struct
  let f ?x:(y : int = 1) () = y
end
[%%expect {|
module M_13 : S @@ stateless
|}]

module M_13_error : T = struct
  let f ?x:(y : int = 1) () = y
  let g (?x:(y = 1) : int) () = y
  let h (?x:(y = 1) : int) () = y
end
[%%expect {|
Line 3, characters 22-25:
3 |   let g (?x:(y = 1) : int) () = y
                          ^^^
Error: Unknown generic optional argument type
|}]

module M_14 : T = struct
  let f ?x:((y : int option) : int option) () = option_get y
  let g (?x:(y : int option) : int option) () = option_get y
  let h (?x:(y : int or_null) : int or_null) () = or_null_get y
end
[%%expect {|
module M_14 : T
|}]

module M_15 : T = struct
  let f ?x:(((y : int) : int) = 1) () = y
  let g (?x:((y : int) = 1) : int option) () = y
  let h (?x:((y : int) = 1) : int or_null) () = y
end
[%%expect {|
module M_15 : T @@ stateless
|}]


(* Test calling f with different argument styles *)
module ApplyS (M : S) = struct
  let v1 = M.f ()
  let v2 = M.f ~x:42 ()
  let v3 = M.f ?x:None ()
  let v4 = M.f ?x:(Some 99) ()
end
[%%expect {|
module ApplyS :
  functor (M : S) ->
    sig
      val v1 : int @@ stateless
      val v2 : int @@ stateless
      val v3 : int @@ stateless
      val v4 : int @@ stateless
    end
  @@ stateless
|}]

(* Test calling f, g, h with different argument styles *)
module ApplyT (M : T) = struct
  let v1_f = M.f ()
  let v1_g = M.g ()
  let v1_h = M.h ()

  let v2_f = M.f ~x:42 ()
  let v2_g = M.g ~x:42 ()
  let v2_h = M.h ~x:42 ()

  let v3_f = M.f ?x:None ()
  let v3_g = M.g ?x:None ()
  let v3_h = M.h ?x:Null ()

  let v4_f = M.f ?x:(Some 99) ()
  let v4_g = M.g ?x:(Some 99) ()
  let v4_h = M.h ?x:(This 99) ()  (* This works because of optional compatibility *)
end
[%%expect {|
module ApplyT :
  functor (M : T) ->
    sig
      val v1_f : int @@ stateless
      val v1_g : int @@ stateless
      val v1_h : int @@ stateless
      val v2_f : int @@ stateless
      val v2_g : int @@ stateless
      val v2_h : int @@ stateless
      val v3_f : int @@ stateless
      val v3_g : int @@ stateless
      val v3_h : int @@ stateless
      val v4_f : int @@ stateless
      val v4_g : int @@ stateless
      val v4_h : int @@ stateless
    end
  @@ stateless
|}]

module Test = ApplyS(M_1)
module Test = ApplyS(M_2)
module Test = ApplyS(M_3)
module Test = ApplyT(M_4)
module Test = ApplyT(M_5)
module Test = ApplyS(M_6)
module Test = ApplyS(M_7)
module Test = ApplyS(M_8)
module Test = ApplyT(M_9)
module Test = ApplyT(M_10)
module Test = ApplyT(M_12)
module Test = ApplyS(M_13)
module Test = ApplyT(M_14)
module Test = ApplyT(M_15)

[%%expect{|
module Test :
  sig
    val v1 : int @@ stateless
    val v2 : int @@ stateless
    val v3 : int @@ stateless
    val v4 : int @@ stateless
  end
module Test :
  sig
    val v1 : int @@ stateless
    val v2 : int @@ stateless
    val v3 : int @@ stateless
    val v4 : int @@ stateless
  end
module Test :
  sig
    val v1 : int @@ stateless
    val v2 : int @@ stateless
    val v3 : int @@ stateless
    val v4 : int @@ stateless
  end
module Test :
  sig
    val v1_f : int @@ stateless
    val v1_g : int @@ stateless
    val v1_h : int @@ stateless
    val v2_f : int @@ stateless
    val v2_g : int @@ stateless
    val v2_h : int @@ stateless
    val v3_f : int @@ stateless
    val v3_g : int @@ stateless
    val v3_h : int @@ stateless
    val v4_f : int @@ stateless
    val v4_g : int @@ stateless
    val v4_h : int @@ stateless
  end
module Test :
  sig
    val v1_f : int @@ stateless
    val v1_g : int @@ stateless
    val v1_h : int @@ stateless
    val v2_f : int @@ stateless
    val v2_g : int @@ stateless
    val v2_h : int @@ stateless
    val v3_f : int @@ stateless
    val v3_g : int @@ stateless
    val v3_h : int @@ stateless
    val v4_f : int @@ stateless
    val v4_g : int @@ stateless
    val v4_h : int @@ stateless
  end
module Test :
  sig
    val v1 : int @@ stateless
    val v2 : int @@ stateless
    val v3 : int @@ stateless
    val v4 : int @@ stateless
  end
module Test :
  sig
    val v1 : int @@ stateless
    val v2 : int @@ stateless
    val v3 : int @@ stateless
    val v4 : int @@ stateless
  end
module Test :
  sig
    val v1 : int @@ stateless
    val v2 : int @@ stateless
    val v3 : int @@ stateless
    val v4 : int @@ stateless
  end
module Test :
  sig
    val v1_f : int @@ stateless
    val v1_g : int @@ stateless
    val v1_h : int @@ stateless
    val v2_f : int @@ stateless
    val v2_g : int @@ stateless
    val v2_h : int @@ stateless
    val v3_f : int @@ stateless
    val v3_g : int @@ stateless
    val v3_h : int @@ stateless
    val v4_f : int @@ stateless
    val v4_g : int @@ stateless
    val v4_h : int @@ stateless
  end
module Test :
  sig
    val v1_f : int @@ stateless
    val v1_g : int @@ stateless
    val v1_h : int @@ stateless
    val v2_f : int @@ stateless
    val v2_g : int @@ stateless
    val v2_h : int @@ stateless
    val v3_f : int @@ stateless
    val v3_g : int @@ stateless
    val v3_h : int @@ stateless
    val v4_f : int @@ stateless
    val v4_g : int @@ stateless
    val v4_h : int @@ stateless
  end
module Test :
  sig
    val v1_f : int @@ stateless
    val v1_g : int @@ stateless
    val v1_h : int @@ stateless
    val v2_f : int @@ stateless
    val v2_g : int @@ stateless
    val v2_h : int @@ stateless
    val v3_f : int @@ stateless
    val v3_g : int @@ stateless
    val v3_h : int @@ stateless
    val v4_f : int @@ stateless
    val v4_g : int @@ stateless
    val v4_h : int @@ stateless
  end
module Test :
  sig
    val v1 : int @@ stateless
    val v2 : int @@ stateless
    val v3 : int @@ stateless
    val v4 : int @@ stateless
  end
module Test :
  sig
    val v1_f : int @@ stateless
    val v1_g : int @@ stateless
    val v1_h : int @@ stateless
    val v2_f : int @@ stateless
    val v2_g : int @@ stateless
    val v2_h : int @@ stateless
    val v3_f : int @@ stateless
    val v3_g : int @@ stateless
    val v3_h : int @@ stateless
    val v4_f : int @@ stateless
    val v4_g : int @@ stateless
    val v4_h : int @@ stateless
  end
module Test :
  sig
    val v1_f : int @@ stateless
    val v1_g : int @@ stateless
    val v1_h : int @@ stateless
    val v2_f : int @@ stateless
    val v2_g : int @@ stateless
    val v2_h : int @@ stateless
    val v3_f : int @@ stateless
    val v3_g : int @@ stateless
    val v3_h : int @@ stateless
    val v4_f : int @@ stateless
    val v4_g : int @@ stateless
    val v4_h : int @@ stateless
  end
|}]
