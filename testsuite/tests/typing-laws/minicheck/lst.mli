(* A list module with laws. The laws are restated in lst.ml, and the
   generated files come from the .mli. *)
val length : 'a list -> int
val eq : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
val zip : 'a list * 'b list -> ('a * 'b) list
val unzip : ('a * 'b) list -> 'a list * 'b list
val init : int -> f:(int -> 'a) -> 'a list

law? init_length (n : int) (f : int -> 'a) :
  n >= 0 ===> length (init n ~f) = n

law? length_nonneg (xs : 'a list) : length xs >= 0

law? trivial : 1 = 1
