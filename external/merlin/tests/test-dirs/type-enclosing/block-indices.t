Test hovering over the labels of block indices.

  $ file="test.ml"

  $ print_merlin_result () {
  >   result="$1"
  >   line=$(echo "$result" | jq '.start.line')
  >   start=$(echo "$result" | jq '.start.col')
  >   end=$(echo "$result" | jq '.end.col')
  >   
  >   start_for_cut=$((start + 1))
  >   end_for_cut=$((end))
  >   value=$(sed -n "${line}p" "$file" | cut -c "${start_for_cut}-${end_for_cut}")
  >   type=$(echo "$result" | jq '.type' -r)
  >   echo "\"$value\" : \"$type\""
  > }

  $ hover () {
  >   line="$1"
  >   col="$2"
  >   
  >   # Print the location we are hovering
  >   sed -n "${line}p" "$file"
  >   printf '%*s^\n' "$col" ''
  >   
  >   # Then print the output from Merlin
  >   $MERLIN single type-enclosing -position "$line:$col" -filename "$file" < "$file" \
  >     | jq -c ".value[]" \
  >     | while read -r result; do
  >         print_merlin_result "$result"
  >       done
  > }

  $ cat > "$file" << EOF
  > type r = { a : int; mutable b : string }
  > type u = #{ x : int; y : float# }
  > type s = { mutable c : u }
  > let i1 = (.a)
  > let i2 = (.c.#y)
  > let i3 = (.idx_mut((.c)).#x)
  > EOF

  $ hover 4 11
  let i1 = (.a)
             ^
  "a" : "int"
  "(.a)" : "(r, int) idx_imm"

  $ hover 5 11
  let i2 = (.c.#y)
             ^
  "c" : "u"
  "(.c.#y)" : "(s, float#) idx_mut"

  $ hover 5 14
  let i2 = (.c.#y)
                ^
  "y" : "float#"
  "(.c.#y)" : "(s, float#) idx_mut"

  $ hover 6 21
  let i3 = (.idx_mut((.c)).#x)
                       ^
  "c" : "u"
  "(.c)" : "(s, u) idx_mut"
  "(.idx_mut((.c)).#x)" : "(s, int) idx_mut"

  $ hover 6 26
  let i3 = (.idx_mut((.c)).#x)
                            ^
  "x" : "int"
  "(.idx_mut((.c)).#x)" : "(s, int) idx_mut"
