(* TEST
 (* CR layouts v5: the bytecode and native toplevels don't currently print
    unboxed values in the same way. Fix that. *)
 (* {
      toplevel;
    } *)
 {
   toplevel.opt;
 }
*)

(* Test 1: Unboxed numbers *)
let a = #0.0;;
let b = #0.0s;;
let c = #0L;;
let d = #0l;;
let e = #0n;;

(* Test 2: Unboxed products *)
type t = #{ x : int; y : int64# };;
let a = #(0, 0);;
let b = #(#0.0, #0L);;
let c = #{ contents = "foo" };;
let d = #{ x = 5; y = #10L };;

(* Test 3: Arrays of unboxed products *)
let a = [| #(0, 0) |];;
let b = [| #{ contents = 1 }; #{ contents = 2 } |];;
let c = [| #{ x = 1; y = #2L } |];;

(* Test 4: Mixed phrases (notice multiple values are bound between each ;;) *)
let x = #42L
let y = 42;;

let foo = "hello"
let unboxed_product = #(#(10, #10.0), "ten", #10L)
let bar = #42L
let baz = "world";;
