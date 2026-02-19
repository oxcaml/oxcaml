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
type t : value non_float mod forkable unyielding many stateless immutable
|}]

type t : immediate
[%%expect {|
type t : value non_pointer mod global many stateless immutable external_
|}]

type t : float64
[%%expect {|
type t : float64 mod external_
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
  : value non_float mod forkable unyielding many stateless immutable with 'a
|}]
