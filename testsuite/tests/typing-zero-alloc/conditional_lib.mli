val apply : ('a -> 'b) -> 'a -> 'b [@@zero_alloc conditional]
val apply2 : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c [@@zero_alloc conditional]

val apply_inline : ('a -> 'b) -> 'a -> 'b [@@zero_alloc conditional]
val apply2_inline : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c [@@zero_alloc conditional]

val identity : 'a -> 'a [@@zero_alloc strict]
val add_ints : int -> int -> int [@@zero_alloc strict]

val allocate_ref : 'a -> 'a ref
val allocate_pair : 'a -> 'b -> 'a * 'b
