let length = Stdlib.List.length
let init n ~f = Stdlib.List.init n f
let zip (xs, ys) = Stdlib.List.combine xs ys
let unzip = Stdlib.List.split
let eq = Stdlib.List.equal

(* The kinds of the type variables are those of the interface: the laws
   file is compiled against it. *)
law? init_length (n : int) (f : int -> ('a : value)) :
  n >= 0 ===> length (init n ~f) = n

law? length_nonneg (xs : ('a : value) list) : length xs >= 0

law? trivial : 1 = 1

law? unzip_zip (xs : int list) (ys : int list) :
  length xs = length ys ===>
  let xs', ys' = unzip (zip (xs, ys)) in
  eq Int.equal xs' xs && eq Int.equal ys' ys
