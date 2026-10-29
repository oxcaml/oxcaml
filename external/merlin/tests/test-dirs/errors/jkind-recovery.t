Issue #6971: type-expression recovery must not add jkind errors.

  $ errors() {
  >   $MERLIN single errors -filename test.ml | revert-newlines \
  >     | jq .value[].message -r
  > }

An invalid or_null argument should not cause a second error on Null.

  $ errors <<'EOF'
  > let v = (Null : float# or_null)
  > EOF
  This type float# should be an instance of type ('a : value_maybe_separable)
  The layout of float# is float64
    because it is the unboxed version of the primitive type float.
  But the layout of float# must be a value layout
    because the type argument of or_null has layout value.

An invalid list argument should not cause a second error on the function body.

  $ errors <<'EOF'
  > let v = (fun x -> x : float# -> float# list)
  > EOF
  This type float# should be an instance of type ('a : value_or_null)
  The layout of float# is float64
    because it is the unboxed version of the primitive type float.
  But the layout of float# must be a value layout
    because the type argument of list has layout value_or_null.

The recovered argument of an external must remain representable.

  $ errors <<'EOF'
  > external f : float# list -> unit = "foo"
  > EOF
  This type float# should be an instance of type ('a : value_or_null)
  The layout of float# is float64
    because it is the unboxed version of the primitive type float.
  But the layout of float# must be a value layout
    because the type argument of list has layout value_or_null.

Recovery should also work inside a let-in expression.

  $ errors <<'EOF'
  > let v = (Null : float# or_null) in ignore v
  > EOF
  This type float# should be an instance of type ('a : value_maybe_separable)
  The layout of float# is float64
    because it is the unboxed version of the primitive type float.
  But the layout of float# must be a value layout
    because the type argument of or_null has layout value.
