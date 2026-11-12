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
  >   | jq -r '.class, (.value | if type == "array" then .[].message else . end)'
  error
  The module M is not a functor, it cannot be applied.
