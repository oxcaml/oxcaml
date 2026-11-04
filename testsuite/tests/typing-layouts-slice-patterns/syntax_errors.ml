(* TEST
   toplevel;
*)

(* A slice pattern has at most one rest *)

(* The error points at the second rest, not at the last one *)
let bad = function [&| ..; x; ..; y; .. |] -> x;;

(* The error points at the second rest only, not at the elements after it *)
let bad = function [&| ..; ..; y |] -> y;;

(* The error points at all of the second rest, including its variable *)
let bad = function [&| ..; .. as r |] -> r;;
