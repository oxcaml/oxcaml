  $ $MERLIN single complete-prefix -position 3:17 \
  > -filename application_context < application_context.ml \
  > | tr '\n' ' ' | jq ".value.context"
  [
    "application",
    {
      "argument_type": "'_weak2",
      "labels": [
        {
          "name": "~j",
          "type": "int"
        },
        {
          "name": "?k",
          "type": "'_weak1 option"
        },
        {
          "name": "~l",
          "type": "lexing_position"
        }
      ]
    }
  ]
