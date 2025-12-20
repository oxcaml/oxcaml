(* TEST
 {
   flags = "-strict-sequence";
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
  | _ #when when_ #() -> case_ #()
  | _ -> #assert #false

[%%expect{|
val f :
  match_:(unit# -> 'a) -> when_:(unit# -> bool#) -> case_:(unit# -> 'b) -> 'b =
  <fun>
|}]
