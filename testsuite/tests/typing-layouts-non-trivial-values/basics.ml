(* TEST
 flags = "-extension layouts_alpha -w +184+185";
 expect;
*)

(* CR layouts-scannable: once annotations aren't being dropped on the floor, the
   printing in the good cases should include more layout information *)

type t : value non_pointer = int
[%%expect{|
type t = int
|}]

type t : immutable_data non_pointer = int
[%%expect{|
type t = int
|}]

type ('a : any non_pointer, 'b : any maybe_pointer, 'c : any) t;;
[%%expect{|
type ('a : any, 'b : any, 'c : any) t
|}]

type t : value non_pointer & value non_pointer = #(int * int)
[%%expect{|
type t = #(int * int)
|}]
