type a = int
type b = a

type pair = int * int

module Foo(X : sig end) = struct type t = T end
