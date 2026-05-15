  $ $MERLIN single complete-prefix -position 3:17 \
  > -filename application_context < application_context.ml \
  > | tr '\n' ' ' | jq ".value.context"
  [
    "application",
    {
<<<<<<< HEAD
      "argument_type": "'b",
||||||| c76379cdae
      "argument_type": "'_weak1",
=======
      "argument_type": "'a",
>>>>>>> v5.6-504
      "labels": [
        {
          "name": "~j",
          "type": "int"
        },
        {
          "name": "?k",
          "type": "'a option"
        },
        {
          "name": "~l",
          "type": "lexing_position"
        }
      ]
    }
  ]
