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
