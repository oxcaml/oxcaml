Ill-typed functor applications should be recoverable errors.

  $ cat > test.ml <<EOF
  > module M = struct end
  > module N = M (struct end)
  > module type S = module type of Unbound (struct end)
  > module F (X : sig type t end) = struct type t = X.t end
  > type t = F(M).t
  > let x : int = "not an int"
  > EOF

  $ $MERLIN single errors -filename test.ml < test.ml \
  >   | revert-newlines \
  >   | jq -r '.value | map(.message) | join("\n---\n")'
  The module M is not a functor, it cannot be applied.
  ---
  Unbound module Unbound
  ---
  The module Unbound is not a functor, it cannot be applied.
  ---
  Modules do not match: sig end
  is not included in sig type t end
  The type t is required but not provided
  File "test.ml", line 4, characters 18-24: Expected declaration
  ---
  This constant has type string but an expression was expected of type int
