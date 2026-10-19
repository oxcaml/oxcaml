type t_void : void

let g (_ : t_void) = ()

let packed = Gadt_extensible.Mk ((fun _ -> assert false), g)
