(* A list module with laws, restated in lst.ml, where the generated file
   comes from. *)
val length : 'a list -> int
val init : int -> f:(int -> 'a) -> 'a list

law? init_length (n : int) (f : int -> 'a) :
  n >= 0 ===> length (init n ~f) = n

law? length_nonneg (xs : 'a list) : length xs >= 0

law? trivial : 1 = 1
