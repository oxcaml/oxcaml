(* TEST
 flags = "-extension layouts_alpha";
 { toplevel; }
 { toplevel.opt; }
*)

module L = struct
        type ('a,'b) t = [] | (::) of 'a * ('b,'a) t
end;;
L.[([1;2]:int list);"2";[3;4];"4";[5]];;
open L;;
[1;"2";3;"4";5];;

module L = struct
        type ('a : any) t = 'a list = [] | (::) of 'a * 'a t
end;;
L.[[1];[2];[3];[4];[5]];;
open L;;
[1;2;3;4;5];;

let floats = [#1.5; #2.5];;
let mixed = [#("first", #1.5); #("second", #2.5)];;
let voids = [#(); #()];;
let rec mixed_cycle = #(42, #1.5) :: mixed_cycle;;
