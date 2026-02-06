(* TEST
 flags = "-kind-verbosity 2";
 expect;
*)

(* CR: We're failing to display top modal bounds and implied modal bounds. *)

(* CR: We should show non-top modal bounds first. *)

type t : value
[%%expect {|
type t
|}]

type t : immutable_data
[%%expect {|
type t
  : value
      mod forkable
          unyielding
          many
          stateless
          immutable
          internal
          non_null
          non_float
|}]

type t : immediate
[%%expect {|
type t
  : value mod global many stateless immutable external_ non_null non_float
|}]

type t : float64
[%%expect {|
type t : float64 mod external_ non_null non_float
|}]

type t : any
[%%expect {|
type t : any mod internal maybe_null maybe_separable
|}]

type t : value mod portable
[%%expect {|
type t : value mod portable internal non_null separable
|}]

type t : value mod stateless
[%%expect {|
type t : value mod stateless internal non_null separable
|}]

type 'a t : immutable_data with 'a
[%%expect {|
type 'a t
  : value
      mod forkable
          unyielding
          many
          stateless
          immutable
          internal
          non_null
          non_float
      with 'a
|}]
