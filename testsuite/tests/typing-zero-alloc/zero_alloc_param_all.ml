(* TEST
   flags = "-w +198";
   expect.opt;
*)

[@@@zero_alloc all];;
[%%expect {|
|}];;

(* Higher-order function: argument must be zero_alloc. *)
let apply (f [@zero_alloc arity 1]) x = f x;;
[%%expect {|
val apply : (('a -> 'b) [@zero_alloc arity 1]) -> 'a -> 'b [@@zero_alloc] =
  <fun>
|}];;

(* Higher-order function: no zero_alloc requirement on the argument. *)
let apply_any f x = f x;;
[%%expect {|
Line 1, characters 14-23:
1 | let apply_any f x = f x;;
                  ^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP3.apply_any (camlTOP3__apply_any_2_3_code).
Line 1, characters 20-23:
1 | let apply_any f x = f x;;
                        ^^^
Error: called function may allocate (indirect tailcall)
|}];;

(* Opted out of zero_alloc all. *)
let[@zero_alloc ignore] apply_ignore f x = f x;;
[%%expect {|
val apply_ignore : ('a -> 'b) -> 'a -> 'b = <fun>
|}];;

(* Argument explicitly ignored: zero_alloc all does not apply to f at the call site. *)
let apply_ignore_arg (f [@zero_alloc ignore]) x = f x;;
[%%expect {|
Line 1, characters 21-53:
1 | let apply_ignore_arg (f [@zero_alloc ignore]) x = f x;;
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP5.apply_ignore_arg (camlTOP5__apply_ignore_arg_6_7_code).
Line 1, characters 50-53:
1 | let apply_ignore_arg (f [@zero_alloc ignore]) x = f x;;
                                                      ^^^
Error: called function may allocate (indirect tailcall)
|}];;

(* Both the function and its argument are ignored. *)
let[@zero_alloc ignore] apply_both_ignore (f [@zero_alloc ignore]) x = f x;;
[%%expect {|
val apply_both_ignore : ('a -> 'b) -> 'a -> 'b = <fun>
|}];;
