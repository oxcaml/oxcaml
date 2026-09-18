(* [include Hof_intf.S] re-exports a shared declaration: it cannot be individually
   exempted, and a floating clause would silently apply to it. Any include therefore
   blocks hoisting at its level; suggestions stay per-item despite the majority. *)
let app f x =
  let _ = f x in
  ()
;;

let double x = x * 2
let triple x = x * 3
let quad x = x * 4
