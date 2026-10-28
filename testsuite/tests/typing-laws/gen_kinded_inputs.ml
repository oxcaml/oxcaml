(* The inputs of the laws of gen_kinded.ml *)
let nullable = Gen_kinded_laws.Nullable_input { x = Null }
let plain = Gen_kinded_laws.Plain_input { x = 1 }
let kinded = Gen_kinded_laws.Kinded_input { x = 1 }
let refined = Gen_kinded_laws.Refined_input { w = Gen_kinded.Int; x = 1 }
