module Basic_int = (Basic (P)) (P_int) [@jane.non_erasable.instances]

let basic = Basic_int.create ()
