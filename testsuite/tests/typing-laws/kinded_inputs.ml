(* The inputs of the laws over kinded type variables, nullable and plain
   ones included (see gen_kinded.ml) *)
let nullable = Kinded_laws.Nullable_input { x = Null }
let plain = Kinded_laws.Plain_input { x = 1 }
let kinded = Kinded_laws.Kinded_input { x = 1 }
