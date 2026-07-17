(* [Bundle_pfree.Intf] is P-free, so [Intf(P_int).S = Intf(P_int_alt).S]
   and [Make(P_int)()] satisfies [Intf(P_int_alt).S]. *)

module R : Bundle_pfree.Intf(P_int_alt).S = Bundle_pfree.Make (P_int) ()

let () = print_endline (R.Basic_pfree.hello ())
