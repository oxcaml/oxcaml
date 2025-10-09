(* TEST
 (* CR layouts v5: the bytecode and native toplevels don't currently print
    unboxed values in the same way. Fix that. *)
 (*{
   toplevel;
 }*){
   toplevel.opt;
 }
*)

(* Test 1: Unboxed numbers *)
let a_1 = #0.0;;
let b_1 = #0.0s;;
let c_1 = #0L;;
let d_1 = #0l;;
let e_1 = #0n;;

(* CR layouts v5: Printing toplevel unboxed products usually causes a segfault
   on native. Fix that. *)
(* Test 2: Unboxed products *)
type t_2 = #{ x : int; y : int64# };;
(* let a_2 = #(0, 0);; *)
(* let b_2 = #(#0.0, #0L);; *)
(* let c_2 = #{ contents = "foo" };; *)
(* let d_2 = #{ x = 5; y = #10L };; *)

(* Test 3: Arrays of unboxed products *)
let a_3 = [| #(0, 0) |];;
let b_3 = [| #{ contents = 1 }; #{ contents = 2 } |];;
let c_3 = [| #{ x = 1; y = #2L } |];;
