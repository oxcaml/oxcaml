(* TEST
 flags = "-kind-verbosity 0";
 expect;
*)

type t : value
[%%expect {|
type t
|}]

type t : immutable_data
[%%expect {|
type t : immutable_data
|}]

type t : immediate
[%%expect {|
type t : immediate
|}]

type t : float64
[%%expect {|
type t : float64
|}]

type t : any
[%%expect {|
type t : any
|}]

type t : value mod portable
[%%expect {|
type t : value mod portable
|}]

type t : value mod stateless
[%%expect {|
type t : value mod stateless
|}]

type 'a t : immutable_data with 'a
[%%expect {|
type 'a t : immutable_data with 'a
|}]

type ('a : immutable_data) t
[%%expect {|
type ('a : immutable_data) t
|}]

type ('a : value mod stateless) t
[%%expect {|
type ('a : value mod stateless) t
|}]
