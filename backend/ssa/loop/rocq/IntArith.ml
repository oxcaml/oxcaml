(** val add_fits : int -> int -> bool **)

let add_fits =
 fun a b ->
  let s = a + b in
  a lxor s land (b lxor s) >= 0

(** val mul_fits : int -> int -> bool **)

let mul_fits =
 fun a b ->
  if Int.equal a 0 || Int.equal b 0
  then true
  else if
    (Int.equal a (-1) && Int.equal b Int.min_int)
    || (Int.equal b (-1) && Int.equal a Int.min_int)
  then false
  else Int.equal (a * b / a) b

(** val add_ovf : int -> int -> int option **)

let add_ovf a b = if add_fits a b then Some (a + b) else None

(** val mul_ovf : int -> int -> int option **)

let mul_ovf a b = if mul_fits a b then Some (a * b) else None

(** val neg_ovf : int -> int option **)

let neg_ovf a = if Int.equal a Int.min_int then None else Some (Int.neg a)

(** val add_fits64 : int64 -> int64 -> bool **)

let add_fits64 =
 fun a b ->
  let s = Int64.add a b in
  Int64.compare (Int64.logand (Int64.logxor a s) (Int64.logxor b s)) 0L >= 0

(** val mul_fits64 : int64 -> int64 -> bool **)

let mul_fits64 =
 fun a b ->
  if Int64.equal a 0L || Int64.equal b 0L
  then true
  else if
    (Int64.equal a (-1L) && Int64.equal b Int64.min_int)
    || (Int64.equal b (-1L) && Int64.equal a Int64.min_int)
  then false
  else Int64.equal (Int64.div (Int64.mul a b) a) b

(** val add_ovf64 : int64 -> int64 -> int64 option **)

let add_ovf64 a b = if add_fits64 a b then Some (Int64.add a b) else None

(** val mul_ovf64 : int64 -> int64 -> int64 option **)

let mul_ovf64 a b = if mul_fits64 a b then Some (Int64.mul a b) else None

(** val int64_is_int : int64 -> bool **)

let int64_is_int n = Int64.equal (Int64.of_int (Int64.to_int n)) n

(** val to_int_ovf : int64 -> int option **)

let to_int_ovf x = if int64_is_int x then Some (Int64.to_int x) else None

(** val gcd64 : int64 -> int64 -> int64 **)

let rec gcd64 a b = if Int64.equal b 0L then a else gcd64 b (Int64.rem a b)

(** val floor_div64 : int64 -> int64 -> int64 **)

let floor_div64 dividend divisor =
  let q = Int64.div dividend divisor in
  if
    Int64.compare dividend 0L < 0
    && not (Int64.equal (Int64.rem dividend divisor) 0L)
  then Int64.sub q 1L
  else q
