type ('a, 'b, 'c) t1 = (y:'b -> x:'a -> 'c) -> 'a -> 'b -> 'c
type ('a, 'b) t21 = (?x:'a -> unit -> 'b) -> 'a -> 'b
type ('a, 'b) t22 = (?x:'a -> unit -> 'b) -> 'a -> 'b
type ('a, 'b) t3 = (?x:'a -> unit -> 'b) -> 'b
type ('a, 'b) t4 = (x:'a -> unit -> 'b) -> 'a -> 'b
type ('a, 'b) tt = (y:'a * x:'b) -> 'a
