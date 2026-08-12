  $ run() {
  >   $MERLIN single kind-enclosing -position $1 \
  >   | jq -r '.value[] | "\(.start.line):\(.start.col)-\(.end.line):\(.end.col): \(.kind)"'
  > }

  $ run 1:5 <<EOF
  > type t = int
  > EOF
  1:0-1:12: immediate

  $ run 1:9 <<EOF
  > type 'a t = int
  > EOF
  1:0-1:15: immediate

  $ run 1:9 <<EOF
  > type t = int option
  > EOF
  1:9-1:12: immediate
  1:9-1:19: immutable_data
  1:0-1:19: immutable_data

  $ run 1:16 <<EOF
  > type t = int option
  > EOF
  1:13-1:19: immutable_data with 'a
  1:9-1:19: immutable_data
  1:0-1:19: immutable_data

  $ run 1:18 <<EOF
  > type 'a t = 'a option
  > EOF
  1:15-1:21: immutable_data with 'a
  1:12-1:21: immutable_data with 'a
  1:0-1:21: immutable_data with 'a

  $ run 1:44 <<EOF
  > type 'a my_list = Nil | Cons of 'a * 'a my_list
  > EOF
  1:40-1:47: any
  1:37-1:47: any
  1:0-1:47: immutable_data with 'a

  $ run 1:24 <<EOF
  > type 'a t1 = Rec of 'a t2 | Leaf of 'a
  > and 'a t2 = Rec of 'a t1 | None
  > EOF
  1:23-1:25: any
  1:20-1:25: any
  1:0-1:38: immutable_data with 'a

  $ run 1:22 <<EOF
  > type 'a t1 = Rec of 'a t2 | Leaf of 'a
  > and 'a t2 = Rec of 'a t1 | None
  > EOF
  1:20-1:22: value
  1:20-1:25: any
  1:0-1:38: immutable_data with 'a

  $ run 2:6 <<EOF
  > type 'a t1
  > type t2 = Foo of int t1
  > EOF
  2:0-2:23: immutable_data with int t1

  $ run 1:14 <<EOF
  > let f (foo : int) =
  >   foo
  > EOF
  1:13-1:16: immediate
  1:13-1:16: immediate
  1:6-2:5: value non_float mod aliased immutable

  $ run 2:4 <<EOF
  > let f (foo : int) =
  >   foo
  > EOF
  2:2-2:5: immediate
  1:6-2:5: value non_float mod aliased immutable
