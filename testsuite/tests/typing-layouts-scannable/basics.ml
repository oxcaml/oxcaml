(* TEST
 flags = "-extension layouts_alpha -w +184+185";
 expect;
*)

(* CR layouts-scannable: These tests should test out the built-ins once they
   get updated to be appropriately non_pointer *)

type t : value non_pointer
[%%expect{|
type t : value non_pointer
|}]

type t : immutable_data non_pointer
[%%expect{|
type t : immutable_data non_pointer
|}]

type ('a : any non_pointer, 'b : any maybe_pointer, 'c : any) t;;
[%%expect{|
type ('a : any non_pointer, 'b : any, 'c : any) t
|}]

type t : value non_pointer & value maybe_pointer & float64
(* CR zeisbach: this should not be printing as float64 maybe_pointer...! *)
[%%expect{|
type t : value non_pointer & value & float64 maybe_pointer
|}]
