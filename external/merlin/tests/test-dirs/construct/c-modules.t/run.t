Simple module construction
  $ $MERLIN single construct -position 45:16 \
  > -filename module.ml <module.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 45,
          "col": 15
        },
        "end": {
          "line": 45,
          "col": 16
        }
      },
      [
        "struct
    type t = private b
    and b =
      | A 
      | B of t 
    type (-!'a, +!'b) t' =
      | T of (('a) -> 'b) 
    type t2 =
      | A 
      | B of string 
      | C of t 
    type nonrec r = {
      lbl1: t ;
      lbl2: float list }
    type nonrec n = r
    and m = float
    type t_ext = ..
    type t_ext +=  
      | Str of string 
    type t_ext +=  
      | A 
    type v = [ `A of t_ext ]
    type t_function =
      (int) -> foo:(string) -> ?bar:(bool option) -> here:[%call_pos ] -> char
    let i = _
    let f = _
    module Sub = struct let y = _ end
    [@@@ocaml.text
      \"Construct does not handle class types yet. Please replace this comment by [room]'s definition.\"]
    [@@@ocaml.text
      \"Construct does not handle classes yet. Please replace this comment by [croom]'s definition.\"]
    module type Another  = sig val i : int end
    module type Sig  =
      sig
        type t
        and b
        val f : (int) -> float
        module type STyp  = sig  end
        module D : Another
        val trepr : 'a . ('a) -> 'a
      end
    module Submod =
      struct
        type t
        and b
        let f = _
        module type STyp  = sig  end
        module D = struct let i = _ end
        let trepr = _
      end
    module SubFunc(M:Sig) = struct let g = _ end
    kind_ k_abstract
    kind_ k_concrete = any
  end"
      ]
    ],
    "notifications": []
  }

First class modules

  $ $MERLIN single construct -position 47:26 \
  > -filename module.ml <module.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 47,
          "col": 25
        },
        "end": {
          "line": 47,
          "col": 26
        }
      },
      [
        "((module struct type t = int end) : (module Small))"
      ]
    ],
    "notifications": []
  }

  $ $MERLIN single construct -position 49:17 \
  > -filename module.ml <module.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 49,
          "col": 16
        },
        "end": {
          "line": 49,
          "col": 17
        }
      },
      [
        "struct type t = int end"
      ]
    ],
    "notifications": []
  }


Construction in functor application
  $ $MERLIN single construct -position 7:22 \
  > -filename functor_app.ml <functor_app.ml | jq '.value'
  [
    {
      "start": {
        "line": 7,
        "col": 21
      },
      "end": {
        "line": 7,
        "col": 22
      }
    },
    [
      "struct let x = _ end"
    ]
  ]
