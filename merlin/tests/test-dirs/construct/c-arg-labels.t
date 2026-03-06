Check completion of values that are functions with various label types :

  $ cat >c1.ml <<EOF
  > let f_unlabeled_arg x = Some x
  > let f_labeled_arg ~x = Some x
  > let f_optional_arg ?(x=5) = Some x
  > let f_position_arg ~(pos : [%call_pos]) x = Some x
  > let x : int option = _
  > EOF

  $ $MERLIN single construct -with-values local -position 5:22 \
  > -filename c1.ml <c1.ml | jq ".value"
  [
    {
      "start": {
        "line": 5,
        "col": 21
      },
      "end": {
        "line": 5,
        "col": 22
      }
    },
    [
      "None",
      "(Some _)",
      "(f_unlabeled_arg _)",
      "(f_labeled_arg ~x:_)",
      "(f_optional_arg ?x:_)",
      "(f_position_arg _)"
    ]
  ]

Check completion of types that are functions with various label types :

  $ cat >c1.ml <<EOF
  > let f_unlabeled_arg x = Some x
  > let f_labeled_arg ~x = Some x
  > let f_optional_arg ?(x=5) = Some x
  > let f_position_arg ~(pos : [%call_pos]) x = Some x
  > let x : int option = _
  > EOF

  $ $MERLIN single construct -with-values local -position 5:22 \
  > -filename c1.ml <c1.ml | jq ".value"
  [
    {
      "start": {
        "line": 5,
        "col": 21
      },
      "end": {
        "line": 5,
        "col": 22
      }
    },
    [
      "None",
      "(Some _)",
      "(f_unlabeled_arg _)",
      "(f_labeled_arg ~x:_)",
      "(f_optional_arg ?x:_)",
      "(f_position_arg _)"
    ]
  ]
