(* TEST
 flags = "-kind-verbosity 1";
 expect;
*)

type t : value
[%%expect {|
type t
|}]

type t : immutable_data
[%%expect {|
type t : value mod forkable unyielding many stateless immutable non_float
|}]

type t : immediate
[%%expect {|
type t : value mod global many stateless immutable external_ non_float
|}]

type t : float64
[%%expect {|
type t : float64 mod external_ non_float
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
type 'a t
  : value mod forkable unyielding many stateless immutable non_float with 'a
|}]
