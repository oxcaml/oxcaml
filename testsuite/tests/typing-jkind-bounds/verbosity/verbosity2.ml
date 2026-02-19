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
  : value non_float
      mod forkable
          unyielding
          many
          stateless
          immutable
          portable
          contended
          local
          unique
          static
          internal
|}]

type t : immediate
[%%expect {|
type t
  : value non_pointer
      mod global
          many
          stateless
          immutable
          forkable
          unyielding
          aliased
          portable
          contended
          external_
          static
|}]

type t : float64
[%%expect {|
type t
  : float64
      mod external_
          local
          unforkable
          yielding
          once
          stateful
          nonportable
          unique
          read_write
          uncontended
          static
|}]

type t : any
[%%expect {|
type t
  : any
      mod local
          unforkable
          yielding
          once
          stateful
          nonportable
          unique
          read_write
          uncontended
          static
          internal
|}]

type t : value mod portable
[%%expect {|
type t
  : value
      mod portable
          local
          unforkable
          yielding
          once
          stateful
          unique
          read_write
          uncontended
          static
          internal
|}]

type t : value mod stateless
[%%expect {|
type t
  : value
      mod stateless
          portable
          local
          unforkable
          yielding
          once
          unique
          read_write
          uncontended
          static
          internal
|}]

type 'a t : immutable_data with 'a
[%%expect {|
type 'a t
  : value non_float
      mod forkable
          unyielding
          many
          stateless
          immutable
          portable
          contended
          local
          unique
          static
          internal
      with 'a
|}]
