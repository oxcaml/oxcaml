val assoc_default : 'a1 -> int -> (int * 'a1) list -> 'a1

val count_occurrences : ('a1 -> bool) -> 'a1 list -> int

val sort_uniq : int list -> int list

val length_at_most : int -> 'a1 list -> bool

val map_opt : ('a1 -> 'a2 option) -> 'a1 list -> 'a2 list option

val filter_map_opt : ('a1 -> 'a2 option option) -> 'a1 list -> 'a2 list option

val nth_int : 'a1 list -> int -> 'a1 option

val same_length : 'a1 list -> 'a2 list -> bool
