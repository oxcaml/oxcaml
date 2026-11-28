Test completing the labels of block indices.

  $ cat > test.ml << EOF
  > type r = { abc : int; abd : string }
  > type u = #{ xyz : int; xyw : string }
  > type s = { c : u }
  > let i1 = (.abc)
  > let i2 = (.c.#xyz)
  > EOF

  $ $MERLIN single complete-prefix -position 4:13 -prefix ab -doc n \
  > -filename test.ml < test.ml | jq -c '.value.entries[] | [.name, .desc]'
  ["abd","r -> string"]
  ["abc","r -> int"]

  $ $MERLIN single complete-prefix -position 5:16 -prefix xy -doc n \
  > -filename test.ml < test.ml | jq -c '.value.entries[] | [.name, .desc]'
  ["xyw","u -> string"]
  ["xyz","u -> int"]

Completion also works when the label is incomplete.

  $ cat > test.ml << EOF
  > type r = { abc : int; mutable abd : string }
  > type r2 = { abx : int }
  > type u = #{ xyz : int; xyw : string }
  > type s = { c : u }
  > let i1 = (.ab)
  > let i2 : (r, _) idx_imm = (.ab)
  > let i3 = (.c.#xy)
  > let i4 : (r, string) idx_mut = (.ab)
  > EOF

  $ $MERLIN single complete-prefix -position 5:13 -prefix ab -doc n \
  > -filename test.ml < test.ml | jq -c '.value.entries[] | [.name, .desc]'
  ["abd","r -> string"]
  ["abx","r2 -> int"]
  ["abc","r -> int"]

  $ $MERLIN single complete-prefix -position 6:30 -prefix ab -doc n \
  > -filename test.ml < test.ml | jq -c '.value.entries[] | [.name, .desc]'
  ["abd","r -> string"]
  ["abc","r -> int"]

  $ $MERLIN single complete-prefix -position 7:16 -prefix xy -doc n \
  > -filename test.ml < test.ml | jq -c '.value.entries[] | [.name, .desc]'
  ["xyw","u -> string"]
  ["xyz","u -> int"]

  $ $MERLIN single complete-prefix -position 8:35 -prefix ab -doc n \
  > -filename test.ml < test.ml | jq -c '.value.entries[] | [.name, .desc]'
  ["abd","r -> string"]
  ["abc","r -> int"]
