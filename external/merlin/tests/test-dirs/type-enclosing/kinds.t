Test that kinds get printed correctly.

  $ type_enclosing () {
  >   local filename=test.ml
  >   cat > "$filename"
  >   local pos="$1"
  > 
  >   local prev_output=""
  >   local verbosity=0
  >   while true; do
  >     local current_output=$($MERLIN single type-enclosing -position "$pos" -verbosity "$verbosity" -filename "$filename" < "$filename" \
  >       | revert-newlines | jq .value[0].type -r)
  >     
  >     # Check if current output matches previous output
  >     if [ "$current_output" = "$prev_output" ]; then
  >       break
  >     else
  >       echo "Verbosity $verbosity:"
  >       echo "$current_output"
  >       prev_output="$current_output"
  >       verbosity=$((verbosity + 1))
  >     fi
  >   done
  > }

Abstract types
  $ type_enclosing 2:11 <<EOF
  > type t
  > type foo = t
  > EOF
  Verbosity 0:
  type t

  $ type_enclosing 2:11 <<EOF
  > type t : immutable_data
  > type foo = t
  > EOF
  Verbosity 0:
  type t : immutable_data

Types with kinds (kinds, not jkinds)
  $ type_enclosing 2:11 <<EOF
  > type t = Foo of { mutable foo : int }
  > type foo = t
  > EOF
  Verbosity 0:
  type t = Foo of { mutable foo : int; }
  Verbosity 1:
  type t : mutable_data = Foo of { mutable foo : int; }

  $ type_enclosing 3:17 <<EOF 
  > type 'a t1 = { foo : 'a }
  > type 'a t2 = 'a t1 = { foo : 'a }
  > type foo = int t2
  > EOF
  Verbosity 0:
  type 'a t2 = 'a t1 = { foo : 'a; }
  Verbosity 1:
  type 'a t2 : immutable_data with 'a = 'a t1 = { foo : 'a; }

Non-Tconstr types
  $ type_enclosing 2:11 <<EOF
  > type t = int * string
  > type foo = t
  > EOF
  Verbosity 0:
  type t = int * string
  Verbosity 1:
  type t : immutable_data with int with string = int * string

  $ type_enclosing 2:11 <<EOF
  > type t = [\`Foo | \`Bar of int]
  > type foo = t
  > EOF
  Verbosity 0:
  type t = [ `Bar of int | `Foo ]
  Verbosity 1:
  type t : immutable_data with int = [ `Bar of int | `Foo ]

Module types
# CR-someday: Make kinds get printed when verbosity is 1
  $ type_enclosing 8:11 <<EOF
  > module M = struct
  >   type t_value
  >   type t_immediate : immediate
  >   type t_record = { foo : int }
  >   type t_kind_and_manifest = t_record = { foo : int }
  >   type t_tuple = int * string
  > end
  > module _ = M
  > EOF
  Verbosity 0:
  sig
    type t_value
    type t_immediate : immediate
    type t_record = { foo : int; }
    type t_kind_and_manifest = t_record = { foo : int; }
    type t_tuple = int * string
  end

First-class modality types, conversions and patterns

  $ cat > modal.ml <<'EOF'
  > type wrapped = (string @@ global)
  > let unwrap (x : wrapped) : string = x
  > let pair (((x, y) as p) : ((int * int) @@ global)) = x + y, p
  > let partial (f : x:int -> y:int -> int) =
  >   ((f ~y:2 : x:int -> int) : (_ @@ global))
  > EOF
  $ $MERLIN single errors -filename modal.ml < modal.ml | jq -c '.value'
  []
  $ $MERLIN single type-enclosing -position 1:17 -filename modal.ml < modal.ml | revert-newlines | jq -c '[.value[].type]'
  ["type string : immutable_data","string","(string @@ global)","type wrapped = (string @@ global)"]
  $ $MERLIN single type-enclosing -position 2:36 -filename modal.ml < modal.ml | revert-newlines | jq -c '[.value[].type]'
  ["wrapped","wrapped","string","wrapped -> string"]
  $ $MERLIN single type-enclosing -position 3:12 -filename modal.ml < modal.ml | revert-newlines | jq -c '[.value[].type]'
  ["int","int * int","(int * int @@ global)","(int * int @@ global)","(int * int @@ global) -> int * (int * int @@ global)"]
