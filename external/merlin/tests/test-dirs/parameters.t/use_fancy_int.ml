module Basic_int = (Basic (P)) (P_int) [@jane.non_erasable.instances]
module Fancy_int = (Fancy (P)) (P_int) [@jane.non_erasable.instances]

let basic = Basic_int.create ()
let fancy = Fancy_int.create basic
let out () =
  print_endline (Fancy_int.to_string fancy)
