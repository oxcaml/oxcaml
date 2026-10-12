(* A list module with laws. The laws are restated in lst.ml, and the
   generated files come from the .mli. *)
val length : 'a list -> int
val eq : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
val zip : 'a list * 'b list -> ('a * 'b) list
val unzip : ('a * 'b) list -> 'a list * 'b list
val init : int -> f:(int -> 'a) -> 'a list

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
