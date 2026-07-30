These tests ensure the stability of identifier reconstruction
in the presence of underscores.

1.1
  $ $MERLIN single type-enclosing -position 3:2 -filename under.ml <<EOF | \
  > jq '.value'
  > let _foo = 4.2
  > let f () : int =
  >   _foo
  > EOF
  [
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 6
      },
      "type": "float",
      "tail": "no"
    },
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 6
      },
      "type": "float",
      "tail": "no"
    },
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 6
      },
      "type": "int",
      "tail": "no"
    },
    {
      "start": {
        "line": 2,
        "col": 6
      },
      "end": {
        "line": 3,
        "col": 6
      },
      "type": "unit -> int",
      "tail": "no"
    }
  ]

1.2
  $ $MERLIN single type-enclosing -position 3:3 -filename under.ml <<EOF | \
  >  jq '.value'
  > let _foo = 4.2
  > let f () : int =
  >   _foo
  > EOF
  [
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 6
      },
      "type": "float",
      "tail": "no"
    },
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 6
      },
      "type": "float",
      "tail": "no"
    },
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 6
      },
      "type": "int",
      "tail": "no"
    },
    {
      "start": {
        "line": 2,
        "col": 6
      },
      "end": {
        "line": 3,
        "col": 6
      },
      "type": "unit -> int",
      "tail": "no"
    }
  ]

We try several places in the identifier to check the result stability
2.1
  $ $MERLIN single type-enclosing -position 3:5 -filename under.ml <<EOF | \
  >  jq '.value'
  > let foo_bar = 4.2
  > let f () : int =
  >   foo_bar
  > EOF
  [
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "float",
      "tail": "no"
    },
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "float",
      "tail": "no"
    },
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "int",
      "tail": "no"
    },
    {
      "start": {
        "line": 2,
        "col": 6
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "unit -> int",
      "tail": "no"
    }
  ]

2.2
  $ $MERLIN single type-enclosing -position 3:6 -filename under.ml <<EOF | \
  >  jq '.value'
  > let foo_bar = 4.2
  > let f () : int =
  >   foo_bar
  > EOF
  [
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "float",
      "tail": "no"
    },
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "float",
      "tail": "no"
    },
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "int",
      "tail": "no"
    },
    {
      "start": {
        "line": 2,
        "col": 6
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "unit -> int",
      "tail": "no"
    }
  ]

2.3
  $ $MERLIN single type-enclosing -position 3:7 -filename under.ml <<EOF | \
  >  jq '.value'
  > let foo_bar = 4.2
  > let f () : int =
  >   foo_bar
  > EOF
  [
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "float",
      "tail": "no"
    },
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "float",
      "tail": "no"
    },
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "int",
      "tail": "no"
    },
    {
      "start": {
        "line": 2,
        "col": 6
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "unit -> int",
      "tail": "no"
    }
  ]

2.4
  $ $MERLIN single type-enclosing -position 3:8 -filename under.ml <<EOF
  > let foo_bar = 4.2
  > let f () : int =
  >   foo_bar
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 2
        },
        "end": {
          "line": 3,
          "col": 9
        },
        "type": "float",
        "tail": "no"
      },
      {
        "start": {
          "line": 3,
          "col": 2
        },
        "end": {
          "line": 3,
          "col": 9
        },
        "type": "float",
        "tail": "no"
      },
      {
        "start": {
          "line": 3,
          "col": 2
        },
        "end": {
          "line": 3,
          "col": 9
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 6
        },
        "end": {
          "line": 3,
          "col": 9
        },
        "type": "unit -> int",
        "tail": "no"
      }
    ],
    "notifications": []
  }

3.1
  $ $MERLIN single type-enclosing -position 5:10 -filename under.ml <<EOF
  > let aa = 4.2
  > let f (x) : int = function
  >   | None -> 3
  >   | Some 5 -> 4
  >   | Some _aa -> 4
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 9
        },
        "end": {
          "line": 5,
          "col": 12
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 5,
          "col": 4
        },
        "end": {
          "line": 5,
          "col": 12
        },
        "type": "int option",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 6
        },
        "end": {
          "line": 5,
          "col": 17
        },
        "type": "'a -> int",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single dump -what typedtree -filename under.ml <<EOF
  > let aa = 4.2
  > let f (x) : int = function
  >   | None -> 3
  >   | Some 5 -> 4
  >   | Some _aa -> 4
  > EOF
  {
    "class": "return",
    "value": "[
    structure_item (under.ml[1,0+0]..under.ml[1,0+12])
      Tstr_value Nonrec
      [
        <def>
          pattern (under.ml[1,0+4]..under.ml[1,0+6])
            Tpat_var \"aa\"
            sort value
            value_mode meet(local,once,nonportable,unforkable,yielding,stateful)(modevar#2[global,many,portable,forkable,unyielding,stateless .. global,once,nonportable,unforkable,yielding,stateful]);meet(unique,uncontended,read_write,static,imply(unique,uncontended,read_write,static)(modevar#3[aliased,contended,immutable,dynamic .. unique,uncontended,read_write,static]))
          expression (under.ml[1,0+9]..under.ml[1,0+12])
            Texp_constant Const_float 4.2
      ]
    structure_item (under.ml[2,13+0]..under.ml[5,70+17])
      Tstr_value Nonrec
      [
        <def>
          pattern (under.ml[2,13+4]..under.ml[2,13+5])
            Tpat_var \"f\"
            sort value
            value_mode meet(local,once,nonportable,unforkable,yielding,stateful)(modevar#9[global,many,portable,forkable,unyielding,stateless .. global,once,nonportable,unforkable,yielding,stateful]);meet(unique,uncontended,read_write,static,imply(unique,uncontended,read_write,static)(modevar#a[aliased,contended,immutable,dynamic .. unique,uncontended,read_write,static]))
          expression (under.ml[2,13+6]..under.ml[5,70+17]) ghost
            Texp_function
            alloc_mode id(modevar#b[global,many,portable,forkable,unyielding,stateless .. global,once,nonportable,unforkable,yielding,stateful]);id(modevar#c[aliased,contended,immutable,dynamic .. unique,uncontended,read_write,static])
            id(modevar#13[global,many,portable,forkable,unyielding,stateless .. local,once,nonportable,unforkable,yielding,stateful]);id(modevar#14[aliased,contended,immutable,dynamic .. unique,uncontended,read_write,dynamic])
            []
            [
              Nolabel
              Param_pat
                pattern (under.ml[2,13+6]..under.ml[2,13+9])
                  Tpat_var \"x\"
                  sort '_representable_layout_1
                  value_mode meet(local,once,nonportable,unforkable,yielding,stateful) . local_to_regional_full(modevar#d[global,many,portable,forkable,unyielding,stateless .. local,once,nonportable,unforkable,yielding,stateful]);meet(unique,uncontended,read_write,static,imply(unique,uncontended,read_write,static)(modevar#e[aliased,contended,immutable,dynamic .. unique,uncontended,read_write,static]))
                id(modevar#d[global,many,portable,forkable,unyielding,stateless .. local,once,nonportable,unforkable,yielding,stateful]);id(modevar#e[aliased,contended,immutable,dynamic .. unique,uncontended,read_write,static])
                []
            ]
            Tfunction_cases (under.ml[2,13+18]..under.ml[5,70+17])
              alloc_mode id(modevar#15[global,many,portable,forkable,unyielding,stateless .. local,once,nonportable,unforkable,yielding,stateful]);id(modevar#16[aliased,contended,immutable,dynamic .. unique,uncontended,read_write,static])
              value
              extra (under.ml[2,13+18]..under.ml[5,70+17])
                Texp_constraint
                core_type (under.ml[2,13+12]..under.ml[2,13+15])
                  Ttyp_constr \"int!\"
                  []
              [
                <case>
                  pattern (under.ml[3,40+4]..under.ml[3,40+8])
                    extra (under.ml[3,40+4]..under.ml[3,40+8])
                      Tpat_inspected_type
                        Label_disambiguation
                          Unambiguous
                    Tpat_construct \"None\"
                    []
                    None
                  expression (under.ml[3,40+12]..under.ml[3,40+13])
                    attribute \"merlin.loc\"
                      []
                    Texp_constant Const_int 3
                <case>
                  pattern (under.ml[4,54+4]..under.ml[4,54+10])
                    extra (under.ml[4,54+4]..under.ml[4,54+10])
                      Tpat_inspected_type
                        Label_disambiguation
                          Unambiguous
                    Tpat_construct \"Some\"
                    [
                      pattern (under.ml[4,54+9]..under.ml[4,54+10])
                        Tpat_constant Const_int 5
                    ]
                    None
                  expression (under.ml[4,54+14]..under.ml[4,54+15])
                    attribute \"merlin.loc\"
                      []
                    Texp_constant Const_int 4
                <case>
                  pattern (under.ml[5,70+4]..under.ml[5,70+12])
                    extra (under.ml[5,70+4]..under.ml[5,70+12])
                      Tpat_inspected_type
                        Label_disambiguation
                          Unambiguous
                    Tpat_construct \"Some\"
                    [
                      pattern (under.ml[5,70+9]..under.ml[5,70+12])
                        Tpat_var \"_aa\"
                        sort value
                        value_mode global,many,portable,forkable,unyielding,stateless;unique,uncontended,read_write,dynamic
                    ]
                    None
                  expression (under.ml[5,70+16]..under.ml[5,70+17])
                    attribute \"merlin.loc\"
                      []
                    Texp_constant Const_int 4
              ]
      ]
  ]
  
  
  ",
    "notifications": []
  }
