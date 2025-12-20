(* TEST
 {
   flags = "-extension comprehensions -strict-sequence";
   expect;
 }
*)

let f ~if_ ~then_ = #if if_ #() then then_ #()

[%%expect{|
val f : if_:(unit# -> bool#) -> then_:(unit# -> unit#) -> unit# = <fun>
|}]

let f ~if_ ~then_ ~else_ = #if if_ #() then then_ #() else else_ #()

[%%expect{|
val f :
  if_:(unit# -> bool#) -> then_:(unit# -> 'a) -> else_:(unit# -> 'a) -> 'a =
  <fun>
|}]

let f ~head ~tail = head #()#; tail #()

[%%expect{|
val f : head:(unit# -> unit#) -> tail:(unit# -> 'a) -> 'a = <fun>
|}]

let f ~do_ = #while #true do do_ #() done

[%%expect{|
val f : do_:(unit# -> unit#) -> 'a = <fun>
|}]

let f ~while_ ~do_ = #while while_ #() do do_ #() done

[%%expect{|
val f : while_:(unit# -> bool#) -> do_:(unit# -> unit#) -> unit# = <fun>
|}]

let f ~from_ ~to_ ~do_ = #for _ = from_ #() to to_ #() do do_ #() done

[%%expect{|
val f :
  from_:(unit# -> int) -> to_:(unit# -> int) -> do_:(unit# -> unit#) -> unit =
  <fun>
|}]

let f ~assert_ = #assert (assert_ #())

[%%expect{|
val f : assert_:(unit# -> bool#) -> unit# = <fun>
|}]

let f ~match_ ~when_ ~case_ =
  match match_ #() with
  | x #when when_ x -> case_ x
  | _ -> #assert #false

[%%expect{|
val f : match_:(unit# -> 'a) -> when_:('a -> bool#) -> case_:('a -> 'b) -> 'b =
  <fun>
|}]

let f ~from_ ~to_ ~when_ = [: i for i = from_ #() to to_ #() #when when_ i :]

[%%expect{|
val f :
  from_:(unit# -> int) ->
  to_:(unit# -> int) -> when_:(int -> bool#) -> int iarray = <fun>
|}]

(* Errors when using boxed/tagged types follow *)

let f () = #if true then #()

[%%expect{|
Line 1, characters 15-19:
1 | let f () = #if true then #()
                   ^^^^
Error: This expression has type "bool" but an expression was expected of type
         "bool#"
|}, Principal{|
Line 1, characters 15-19:
1 | let f () = #if true then #()
                   ^^^^
Error: This expression has type "bool" but an expression was expected of type
         "bool#"
       because it is in the condition of an unboxed if-statement
|}]

let f () = #if #true then ()

[%%expect{|
Line 1, characters 26-28:
1 | let f () = #if #true then ()
                              ^^
Error: This expression has type "unit" but an expression was expected of type
         "unit#"
|}, Principal{|
Line 1, characters 26-28:
1 | let f () = #if #true then ()
                              ^^
Error: This expression has type "unit" but an expression was expected of type
         "unit#"
       because it is in the result of an unboxed conditional with no else branch
|}]

let f () = ()#; #()

[%%expect{|
Line 1, characters 11-13:
1 | let f () = ()#; #()
               ^^
Error: This expression has type "unit" but an expression was expected of type
         "unit#"
       because it is in the left-hand side of an unboxed sequence
|}]

let f () = #while true do #() done

[%%expect{|
Line 1, characters 18-22:
1 | let f () = #while true do #() done
                      ^^^^
Error: This expression has type "bool" but an expression was expected of type
         "bool#"
|}, Principal{|
Line 1, characters 18-22:
1 | let f () = #while true do #() done
                      ^^^^
Error: This expression has type "bool" but an expression was expected of type
         "bool#"
       because it is in the condition of an unboxed while-loop
|}]

let f () = #while #true do () done

[%%expect{|
Line 1, characters 27-29:
1 | let f () = #while #true do () done
                               ^^
Error: This expression has type "unit" but an expression was expected of type
         "unit#"
       because it is in the body of an unboxed while-loop
|}]

let f () = #for _ = 0 to 0 do () done

[%%expect{|
Line 1, characters 30-32:
1 | let f () = #for _ = 0 to 0 do () done
                                  ^^
Error: This expression has type "unit" but an expression was expected of type
         "unit#"
       because it is in the body of an unboxed for-loop
|}]

let f () = #assert true

[%%expect{|
Line 1, characters 19-23:
1 | let f () = #assert true
                       ^^^^
Error: This expression has type "bool" but an expression was expected of type
         "bool#"
|}, Principal{|
Line 1, characters 19-23:
1 | let f () = #assert true
                       ^^^^
Error: This expression has type "bool" but an expression was expected of type
         "bool#"
       because it is in the condition of an unboxed assertion
|}]

let f () = match () with () #when true -> () | _ -> assert false

[%%expect{|
Line 1, characters 34-38:
1 | let f () = match () with () #when true -> () | _ -> assert false
                                      ^^^^
Error: This expression has type "bool" but an expression was expected of type
         "bool#"
|}, Principal{|
Line 1, characters 34-38:
1 | let f () = match () with () #when true -> () | _ -> assert false
                                      ^^^^
Error: This expression has type "bool" but an expression was expected of type
         "bool#"
       because it is in an unboxed when-guard
|}]

let f () = [: () for _ = 0 to 0 #when true :]

[%%expect{|
Line 1, characters 38-42:
1 | let f () = [: () for _ = 0 to 0 #when true :]
                                          ^^^^
Error: This expression has type "bool" but an expression was expected of type
         "bool#"
|}, Principal{|
Line 1, characters 38-42:
1 | let f () = [: () for _ = 0 to 0 #when true :]
                                          ^^^^
Error: This expression has type "bool" but an expression was expected of type
         "bool#"
       because it is in an unboxed when-clause in a comprehension
|}]
