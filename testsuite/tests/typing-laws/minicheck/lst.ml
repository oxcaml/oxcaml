let length = Stdlib.List.length
let eq equal xs ys =
  Stdlib.List.length xs = Stdlib.List.length ys
  && Stdlib.List.for_all2 equal xs ys
let zip (xs, ys) = Stdlib.List.combine xs ys
let unzip zs = Stdlib.List.split zs
let init n ~f = Stdlib.List.init n f

law? unzip_zip (xs : int list) (ys : int list) :
  length xs = length ys ===>
  let xs', ys' = unzip (zip (xs, ys)) in
  eq Int.equal xs' xs && eq Int.equal ys' ys

law? zip_unzip (zs : (int * int) list) :
  eq (fun (a, b) (c, d) -> Int.equal a c && Int.equal b d) (zip (unzip zs)) zs

law? init_length (n : int) (f : int -> 'a) :
  n >= 0 ===> length (init n ~f) = n

law? length_nonneg (xs : 'a list) : length xs >= 0

law? trivial : true
