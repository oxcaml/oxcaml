#syntax quotations on

let x = 42

let y = <[Quoted_global_indirect_value.y]>

let z = <[(), fun (x : Quoted_global_indirect_type.t) -> x]>
