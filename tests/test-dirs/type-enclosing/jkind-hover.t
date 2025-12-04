Test that hovering over jkind annotations shows their full expansion.

  $ file="test.ml"

  $ print_merlin_result () {
  >   result="$1"
  >   line=$(echo "$result" | jq '.start.line')
  >   start=$(echo "$result" | jq '.start.col')
  >   end=$(echo "$result" | jq '.end.col')
  >   
  >   start_for_cut=$((start + 1))
  >   end_for_cut=$((end + 1))
  >   value=$(sed -n "${line}p" "$file" | cut -c "${start_for_cut}-${end_for_cut}")
  >   type=$(echo "$result" | jq '.type' -r)
  >   echo "\"$value\" : \"$type\""
  > }

  $ hover () {
  >   line="$1"
  >   col="$2"
  >   enclosings="$3"
  >   
  >   # Print the location we are hovering
  >   sed -n "${line}p" "$file"
  >   printf '%*s^\n' "$col" ''
  >   
  >   # Then print the output from Merlin
  >   $MERLIN single type-enclosing -position "$line:$col" -filename "$file" < "$file" \
  >     | jq -c ".value[:$enclosings][]" \
  >     | while read -r result; do
  >         print_merlin_result "$result"
  >       done
  > }

  $ cat > "$file" << EOF
  > type t1 : immutable_data
  > type t2 : value mod portable
  > type ('a : immediate) t3 : value
  > type 'a t4 : immutable_data mod global with 'a
  > type t5 : value mod everything
  > type t6 : bits32
  > type t7 : bits32 mod portable contended
  > type t8 : void
  > module type S = sig
  >   val f : ('a : immediate). 'a -> 'a
  >   val g : ('b : bits32) -> ('b : value mod portable)
  > end
  > EOF

  $ hover 1 14 1
  type t1 : immutable_data
                ^
  "immutable_data" : "value mod forkable unyielding many stateless immutable non_float"

  $ hover 2 11 2
  type t2 : value mod portable
             ^
  "value " : "value"
  "value mod portable" : "value mod portable"

  $ hover 3 16 1
  type ('a : immediate) t3 : value
                  ^
  "immediate)" : "value mod global many stateless immutable external_ non_float"

  $ hover 3 28 2
  type ('a : immediate) t3 : value
                              ^
  "value" : "value"
  "type ('a : immediate) t3 : value" : "type ('a : immediate) t3"

# CR-someday: It'd be nice to print the with-bounds when we enclose the whole jkind
  $ hover 4 20 3
  type 'a t4 : immutable_data mod global with 'a
                      ^
  "immutable_data " : "value mod forkable unyielding many stateless immutable non_float"
  "immutable_data mod global " : "value mod global many stateless immutable non_float"
  "type 'a t4 : immutable_data mod global with 'a" : "type 'a t4 : immutable_data mod global unforkable yielding with 'a"

  $ hover 5 11 2
  type t5 : value mod everything
             ^
  "value " : "value"
  "value mod everything" : "value mod global many stateless immutable external_"

  $ hover 6 11 1
  type t6 : bits32
             ^
  "bits32" : "bits32 mod non_float"

  $ hover 7 11 2
  type t7 : bits32 mod portable contended
             ^
  "bits32 " : "bits32 mod non_float"
  "bits32 mod portable contended" : "bits32 mod portable contended non_float"

  $ hover 8 11 1
  type t8 : void
             ^
  "void" : "void mod non_float"

  $ hover 10 18 1
    val f : ('a : immediate). 'a -> 'a
                    ^
  "immediate)" : "value mod global many stateless immutable external_ non_float"

  $ hover 11 18 1
    val g : ('b : bits32) -> ('b : value mod portable)
                    ^
  "bits32)" : "bits32 mod non_float"

# CR-someday: This is failing because of poor error recovery.
  $ hover 11 35 2
    val g : ('b : bits32) -> ('b : value mod portable)
                                     ^
  "('b : value mod portable)" : "'a"
  "('b : bits32) -> ('b : value mod portable)" : "'b -> 'a"
