Test locating the labels of block indices.

  $ cat > test.ml << EOF
  > module M = struct
  >   type r = { a : int; mutable b : string }
  > end
  > type u = #{ x : int; y : float# }
  > type s = { mutable c : u }
  > let i1 = (.M.a)
  > let i2 = (.c.#y)
  > let i3 = (.idx_mut((.c)).#x)
  > EOF

  $ locate () {
  >   $MERLIN single locate -look-for ml -position "$1" \
  >     -filename test.ml < test.ml | jq -c '.value.pos'
  > }

  $ locate 6:13
  {"line":2,"col":13}

  $ locate 7:11
  {"line":5,"col":19}

  $ locate 7:14
  {"line":4,"col":21}

  $ locate 8:21
  {"line":5,"col":19}

  $ locate 8:26
  {"line":4,"col":12}

Locating a label uses its type-directed disambiguation:

  $ cat > test.ml << EOF
  > type r1 = { a : int }
  > type r2 = { a : string }
  > type u1 = #{ x : int }
  > type u2 = #{ x : string }
  > type s1 = { c1 : u1 }
  > type s2 = { c2 : u2 }
  > let i1 : (r1, int) idx_imm = (.a)
  > let i2 : (r2, string) idx_imm = (.a)
  > let i3 = (.c1.#x)
  > let i4 = (.c2.#x)
  > EOF

  $ locate 7:31
  {"line":1,"col":12}

  $ locate 8:34
  {"line":2,"col":12}

  $ locate 9:15
  {"line":3,"col":13}

  $ locate 10:15
  {"line":4,"col":13}

Locating a label still works when the index doesn't have the expected type:

  $ cat > test.ml << EOF
  > type r1 = { a : int }
  > type r2 = { a : string }
  > let i : (r2, int) idx_imm = (.a)
  > EOF

  $ locate 3:30
  {"line":2,"col":12}
