let length = Stdlib.List.length
let eq equal xs ys =
  Stdlib.List.length xs = Stdlib.List.length ys
  && Stdlib.List.for_all2 equal xs ys
let zip (xs, ys) = Stdlib.List.combine xs ys
let unzip zs = Stdlib.List.split zs
let init n ~f = Stdlib.List.init n f

law? init_length (n : int) (f : int -> 'a) :
  n >= 0 ===> length (init n ~f) = n

law? length_nonneg (xs : 'a list) : length xs >= 0

law? trivial : 1 = 1
