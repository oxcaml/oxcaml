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
  Verbosity 1: value non_pointer mod global many stateless immutable external_
  Verbosity 2: value non_pointer non_null
    mod global
        many
        stateless
        immutable
        forkable
        unyielding
        aliased
        portable
        contended
        external_
        static

  $ run 1:17 <<EOF
  > type 'a t = 'a option
  > EOF
  Verbosity 0: immutable_data with 'a
  Verbosity 1: value non_float mod forkable unyielding many stateless immutable with 'a
  Verbosity 2: value non_float non_null
    mod forkable
        unyielding
        many
        stateless
        immutable
        portable
        contended
        local
        unique
        static
        internal
    with 'a

  $ run 2:6 <<EOF
  > type 'a t1
  > type t2 = Foo of int t1
  > EOF
  Verbosity 0: immutable_data with int t1
  Verbosity 1: value non_float mod forkable unyielding many stateless immutable with int t1
  Verbosity 2: value non_float non_null
    mod forkable
        unyielding
        many
        stateless
        immutable
        portable
        contended
        local
        unique
        static
        internal
    with int t1

  $ run 1:5 <<EOF
  > type t : value mod portable
  > EOF
  Verbosity 0: value mod portable
  Verbosity 1: value separable non_null
    mod portable
        local
        unforkable
        yielding
        once
        stateful
        unique
        read_write
        uncontended
        static
        internal
  Verbosity 2: value separable non_null
    mod portable
        local
        unforkable
        yielding
        once
        stateful
        unique
        read_write
        uncontended
        static
        internal

  $ run 1:5 <<EOF
  > type t : value mod stateless
  > EOF
  Verbosity 0: value mod stateless
  Verbosity 1: value separable non_null
    mod stateless
        portable
        local
        unforkable
        yielding
        once
        unique
        read_write
        uncontended
        static
        internal
  Verbosity 2: value separable non_null
    mod stateless
        portable
        local
        unforkable
        yielding
        once
        unique
        read_write
        uncontended
        static
        internal
