This test demonstrates Merlin's prioritization of identifiers in various namespaces when
performing completion.

Define a bunch of intentifiers with the name foobar/Foobar in the environment.
  $ cat >env.ml <<EOF
  > type foobar = Foobar
  > type record = { foobar : int }
  > type poly_variant = [ \`Foobar ]
  > module Foobar = struct end
  > module type foobar = sig end
  > let foobar ~foobar = ()
  > EOF

Given a line of ocaml and a column number, return the completions for that line and
column, with the identifiers defined in env.ml in scope.
  $ completions_for () {
  >   line="$1"
  >   col_num="$2"
  >   # Create a file test.ml that is env.ml, but with $line appended to the end of it
  >   cat env.ml > test.ml
  >   echo "$line" >> test.ml
  >   line_num="$(wc -l < test.ml)"
  >   $MERLIN single complete-prefix -position "$line_num":"$col_num" -filename test.ml < test.ml \
  >     | revert-newlines \
  >     | jq '.value.entries | .[] | select(.name == "foobar" or .name == "Foobar")' \
  >     | jq '{name, kind}'
  > }

Completions in an expression context
  $ completions_for "let _ = " 18
  {
    "name": "foobar",
    "kind": "Value"
  }
  {
    "name": "Foobar",
    "kind": "Constructor"
  }
  {
    "name": "Foobar",
    "kind": "Module"
  }
  {
    "name": "foobar",
    "kind": "Type"
  }
  {
    "name": "foobar",
    "kind": "Signature"
  }

Completions in a module context
  $ completions_for "module M = " 11
  {
    "name": "Foobar",
    "kind": "Module"
  }
  {
    "name": "foobar",
    "kind": "Signature"
  }
  {
    "name": "foobar",
    "kind": "Type"
  }
  {
    "name": "foobar",
    "kind": "Value"
  }
  {
    "name": "Foobar",
    "kind": "Constructor"
  }

Completions in a type context
  $ completions_for "type t = " 9
  {
    "name": "foobar",
    "kind": "Type"
  }
  {
    "name": "Foobar",
    "kind": "Module"
  }
  {
    "name": "foobar",
    "kind": "Signature"
  }
  {
    "name": "foobar",
    "kind": "Value"
  }
  {
    "name": "Foobar",
    "kind": "Constructor"
  }

Completions in a module type context.
# CR-someday: Make Merlin infer a module type context
  $ completions_for "module type S = " 16
  {
    "name": "foobar",
    "kind": "Value"
  }
  {
    "name": "Foobar",
    "kind": "Constructor"
  }
  {
    "name": "Foobar",
    "kind": "Module"
  }
  {
    "name": "foobar",
    "kind": "Type"
  }
  {
    "name": "foobar",
    "kind": "Signature"
  }

Completions in a pattern
  $ completions_for "let _ = match _ with | " 23
  {
    "name": "Foobar",
    "kind": "Constructor"
  }
  {
    "name": "Foobar",
    "kind": "Module"
  }
  {
    "name": "foobar",
    "kind": "Label"
  }
  {
    "name": "foobar",
    "kind": "Value"
  }
  {
    "name": "foobar",
    "kind": "Type"
  }
  {
    "name": "foobar",
    "kind": "Signature"
  }

Completions in a structure context
  $ completions_for "module M = struct  end" 18
  {
    "name": "foobar",
    "kind": "Value"
  }
  {
    "name": "Foobar",
    "kind": "Constructor"
  }
  {
    "name": "foobar",
    "kind": "Type"
  }
  {
    "name": "Foobar",
    "kind": "Module"
  }
  {
    "name": "foobar",
    "kind": "Signature"
  }

Completions in a signature context
  $ completions_for "module type S = sig  end" 20
  {
    "name": "foobar",
    "kind": "Type"
  }
  {
    "name": "Foobar",
    "kind": "Module"
  }
  {
    "name": "foobar",
    "kind": "Signature"
  }
  {
    "name": "foobar",
    "kind": "Value"
  }
  {
    "name": "Foobar",
    "kind": "Constructor"
  }
