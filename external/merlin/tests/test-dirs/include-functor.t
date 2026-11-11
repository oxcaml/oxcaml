Test that Merlin handles `include functor` correctly, both with and without
statefulness.

  $ $MERLIN server stop-server

  $ cat > foo.ml <<EOF
  > type t
  > EOF

  $ $MERLIN server errors -filename foo.ml < foo.ml | revert-newlines | jq .value
  []

  $ cat > foo.ml <<EOF
  > type t
  > module F (M : sig type t end) = struct type u = M.t end
  > include functor F
  > EOF

  $ $MERLIN server errors -filename foo.ml < foo.ml | revert-newlines | jq .value
  []

  $ $MERLIN single errors -filename foo.ml < foo.ml | revert-newlines | jq .value
  []

  $ cat > foo.mli <<EOF
  > type t
  > EOF

  $ $MERLIN server errors -filename foo.mli < foo.mli | revert-newlines | jq .value
  []

  $ cat > foo.mli <<EOF
  > type t
  > module type F = functor (M : sig type t end) -> sig type u = M.t end
  > include functor F
  > EOF

  $ $MERLIN server errors -filename foo.mli < foo.mli | revert-newlines | jq .value
  []

  $ $MERLIN single errors -filename foo.mli < foo.mli | revert-newlines | jq .value
  []

  $ $MERLIN server stop-server
