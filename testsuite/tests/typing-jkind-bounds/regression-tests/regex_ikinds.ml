(* TEST
 flags = "-extension small_numbers";
 expect;
*)

type 'a ast1 =
  | Alt : 'a list -> 'a ast1
[%%expect{|
type 'a ast1 = Alt : 'a list -> 'a ast1
|}]

type 'a ast : immutable_data with 'a =
  | Alt : 'a list -> 'a ast
[@@unsafe_allow_any_mode_crossing]
[%%expect{|
type 'a ast : immutable_data with 'a = Alt : 'a list -> 'a ast
[@@unsafe_allow_any_mode_crossing]
|}]

type cset : immutable_data =
  | Cast of cset ast
[%%expect{|
type cset = Cast of cset ast
|}]

module Bar : sig
  type foo : immutable_data
end = struct
  type foo = cset ast
end
[%%expect{|
module Bar : sig type foo : immutable_data end
|}]
