  $ $MERLIN single complete-prefix -position 3:17 \
  > -filename application_context < application_context.ml \
  > | tr '\n' ' ' | jq ".value.context"
  [
    "application",
    {
      "argument_type": "'b",
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
