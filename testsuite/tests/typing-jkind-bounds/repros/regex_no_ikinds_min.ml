(* TEST
 flags = "-extension small_numbers";
 expect;
*)

type ('a, _) ast : immutable_data with 'a =
  | Alt : 'a list -> ('a, [> `Uncased]) ast
  | No : 'a -> ('a, [> `Cased]) ast
  | Case : 'a -> ('a, [> `Cased]) ast
[@@unsafe_allow_any_mode_crossing]

type cset =
  | Intersect of cset list
  | Complement of cset list
  | Difference of cset * cset
  | Cast of (cset, [ `Cased | `Uncased ]) ast

type foo : immutable_data = (cset, [ `Cased | `Uncased ]) ast
[%%expect{|
type ('a, _) ast
  : immutable_data with 'a =
    Alt : 'a list -> ('a, [> `Uncased ]) ast
  | No : 'a -> ('a, [> `Cased ]) ast
  | Case : 'a -> ('a, [> `Cased ]) ast
[@@unsafe_allow_any_mode_crossing]
type cset =
    Intersect of cset list
  | Complement of cset list
  | Difference of cset * cset
  | Cast of (cset, [ `Cased | `Uncased ]) ast
type foo = (cset, [ `Cased | `Uncased ]) ast
|}]

module Bar : sig
  type foo : immutable_data
end = struct
  type foo = (cset, [ `Cased | `Uncased ]) ast
end
[%%expect{|
module Bar : sig type foo : immutable_data end
|}]
