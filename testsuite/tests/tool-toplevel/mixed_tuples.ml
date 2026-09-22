(* TEST
 {
   compiler_reference = "${test_source_directory}/mixed_tuples.byte.reference";
   toplevel;
 }{
   toplevel.opt;
 }
*)

let mixed = (#3.25, "middle", -#42L, 7, #(), -#12s, -#300S);;
let numbers = (-#1l, #2n, #3.5s, -#4.5, #5L, -#6s, #7S);;
let labeled = (~left:#1.25, ~name:"tuple", ~empty:#(), ~right:#2L);;
let nested = ((#1.5, "inner"), #2L, (3, #4.5s));;
let empty = (#(), #());;
let one_slot = (#(), #42L, #());;
let boxed_float = (#1L, 2.5, #3.5);;
let nullable = (#1.5, Null, This "value", #2L);;
type singleton = #{ scalar : float# };;
let singleton = (#{ scalar = #3.5 }, "singleton", #4L);;

let make_mixed x = (#1.5, x, #2L);;
make_mixed [1; 2];;

type cycle = { mutable next : (float# * cycle) option };;
let cycle = { next = None };;
cycle.next <- Some (#1.5, cycle);;
cycle;;

#print_depth 2;;
nested;;
#print_depth 100;;
#print_length 2;;
mixed;;
#print_length 100;;
