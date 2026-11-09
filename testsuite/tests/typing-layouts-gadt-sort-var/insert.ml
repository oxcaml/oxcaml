type t_void : void

let g (_ : t_void) = ()

let packed = Gadt.Mk ((fun _ -> assert false), g)
