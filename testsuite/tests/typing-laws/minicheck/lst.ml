let length = Stdlib.List.length
let init n ~f = Stdlib.List.init n f

(* The kinds of the type variables are those of the interface: the laws
   file is compiled against it. *)
law? init_length (n : int) (f : int -> ('a : value)) :
  n >= 0 ===> length (init n ~f) = n

law? length_nonneg (xs : ('a : value) list) : length xs >= 0

law? trivial : 1 = 1
