  $ cat >main.ml <<EOF
  > let o = object
  >   method a = 1
  >   method b = 2
  >   method cdefg = 2
  > end;;
  > print_int o#a;;
  > print_int o#cdef
  > EOF

  $ $MERLIN single errors -filename main.ml <main.ml |
<<<<<<< HEAD
  > tr '\r\n' ' ' | jq '.value[0] | {message, sub: [.sub[].message]}'
  {
    "message": "This expression has type < a : int; b : int; cdefg : int > It has no method cdef",
    "sub": [
      "Hint:      Did you mean cdefg?"
    ]
  }
||||||| c76379cdae
  > tr '\r\n' ' ' | jq '.value[0].message'
  "This expression has type < a : int; b : int; cdefg : int > It has no method cdef Hint: Did you mean cdefg?"
=======
  > tr '\r\n' ' ' | jq '.value[0]'
  {
    "start": {
      "line": 7,
      "col": 10
    },
    "end": {
      "line": 7,
      "col": 11
    },
    "type": "typer",
    "sub": [
      {
        "start": {
          "line": 7,
          "col": 10
        },
        "end": {
          "line": 7,
          "col": 11
        },
        "message": "Hint: Did you mean cdefg?"
      }
    ],
    "valid": true,
    "message": "This expression has type < a : int; b : int; cdefg : int > It has no method cdef"
  }
>>>>>>> v5.6-504
