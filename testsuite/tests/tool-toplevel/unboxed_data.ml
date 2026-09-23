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
type t = #{ x : int; y : int64_u };;
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

let one_slot_product = #(#(), 42);;
let zero_slot_product = #(#(), #());;

type one_slot_record = { payload : #(unit# * int) };;
let one_slot_record = { payload = #(#(), 42) };;

type inherited_one_slot = { inherit payload : #(unit# * int) };;
let inherited_one_slot = { payload = #(#(), 42) };;

type scalar_record = #{ scalar : float# };;
let scalar_record = #{ scalar = #3.5 };;
let labeled = #(~left:#1.25, ~name:"tuple", ~empty:#(), ~right:#2L);;
let numbers = #(-#12s, -#300S, -#4l, #5n, #6.5s, -#7.5, #8L);;

type nested = #{ a : t; b : #(unit# * string * float#); c : unit# };;
let nested =
  #{ a = #{ x = 7; y = #8L }; b = #(#(), "nested", #9.5); c = #() };;

type ('a : any) poly = #{ data : 'a; empty : unit# };;
let poly = #{ data = #("poly", #10.5); empty = #() };;

let boxed_tuple = (#(#1.5, "tuple"), #2L, nested);;
type variant = Product of #(float# * string) | Record of { data : nested };;
let variant = Product #(#2.5, "variant");;
let inline = Record { data = nested };;

module M = struct
  let product = #(#3.5, "module", #4L)
end;;
M.product;;

type cycle = { mutable next : #(int * cycle) option };;
let cycle = { next = None };;
cycle.next <- Some #(42, cycle);;
cycle;;

#print_depth 2;;
nested;;
#print_depth 100;;
#print_length 2;;
poly;;
#print_length 100;;
