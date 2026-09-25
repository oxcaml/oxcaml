  $ $MERLIN single signature-help -position 2:14 << EOF
  > let f ~x ~y = x + y
  > let _ = 1 - f 
  > EOF
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "f : x:int -> y:int -> int",
          "parameters": [
            {
              "label": [
                4,
                9
              ]
            },
            {
              "label": [
                13,
                18
              ]
            }
          ]
        }
      ],
      "activeParameter": 0,
      "activeSignature": 0
    },
    "notifications": []
  }

  $ $MERLIN single signature-help -position 2:19 << EOF
  > let f ~x ~y = x + y
  > let _ = 1 - f ~x:3 
  > EOF
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "f : x:int -> y:int -> int",
          "parameters": [
            {
              "label": [
                4,
                9
              ]
            },
            {
              "label": [
                13,
                18
              ]
            }
          ]
        }
      ],
      "activeParameter": 1,
      "activeSignature": 0
    },
    "notifications": []
  }
