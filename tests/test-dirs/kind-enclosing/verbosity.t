  $ run() {
  >   cat > test.ml
  >   i=0
  >   while [ "$i" -le 2 ]; do
  >     $MERLIN single kind-enclosing -position "$1" -verbosity "$i" < test.ml \
  >       | revert-newlines \
  >       | jq -r "\"Verbosity $i: \(.value[0].kind)\""
  >     i=$(($i+1))
  >   done
  > }

  $ run 1:9 <<EOF
  > type t = int
  > EOF
  Verbosity 0: immediate
  Verbosity 1: value mod global many stateless immutable external_ non_float
  Verbosity 2: value mod global many stateless immutable external_ non_null non_float

  $ run 1:17 <<EOF
  > type 'a t = 'a option
  > EOF
  Verbosity 0: immutable_data with 'a
  Verbosity 1: value mod forkable unyielding many stateless immutable non_float with 'a
  Verbosity 2: value
    mod forkable
        unyielding
        many
        stateless
        immutable
        internal
        non_null
        non_float
    with 'a

  $ run 2:6 <<EOF
  > type 'a t1
  > type t2 = Foo of int t1
  > EOF
  Verbosity 0: immutable_data with int t1
  Verbosity 1: value mod forkable unyielding many stateless immutable non_float with int t1
  Verbosity 2: value
    mod forkable
        unyielding
        many
        stateless
        immutable
        internal
        non_null
        non_float
    with int t1

  $ run 1:5 <<EOF
  > type t : value mod portable
  > EOF
  Verbosity 0: value mod portable
  Verbosity 1: value mod portable
  Verbosity 2: value mod portable internal non_null separable
