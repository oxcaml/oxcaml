(* TEST
   flags = "-ikinds";
   expect;
*)

type t : immediate with string
[%%expect {|
type t : immutable_data
|}]

module M : sig
  type t : immutable_data
end = struct
  type t = string
end
[%%expect {|
module M : sig type t : immutable_data end
|}]

type q : value
type t : immediate with q
[%%expect{|
type q
type t : immediate with q
|}]

type t : immediate with int
[%%expect{|
type t : immediate
|}]
