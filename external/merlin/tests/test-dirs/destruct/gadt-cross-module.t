  $ mkdir lib
  $ cat >lib/gadt.ml <<EOF
  > type _ u =
  >   | A : A.t u
  >   | B : B.t u
  > type packed = Packed : _ u -> packed
  > type 'a t = 'a u
  > EOF

  $ cat >lib/a.ml <<EOF
  > type t = int
  > EOF

  $ cat >lib/b.ml <<EOF
  > type t = string
  > EOF

  $ $OCAMLC -I lib lib/a.ml lib/b.ml lib/gadt.ml

  $ $MERLIN single case-analysis -I lib -start 3:5 -end 3:5 -filename client.ml <<EOF
  > let f (Packed t : Gadt.packed) =
  >   match (t : _ Gadt.t) with
  >   | _ -> assert false
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 4
        },
        "end": {
          "line": 3,
          "col": 5
        }
      },
      "Gadt.A | Gadt.B"
    ],
    "notifications": []
  }
