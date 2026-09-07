(* Instantiating [Mid] with [P := P_int] substitutes into [Mid]'s signature and
   bound globals, revealing the phantom hidden argument: [outer.cmi] records
   [module C = Clk[P:P_int]] even though [Clk] takes no parameters. *)
module M = Mid(P)(P_int) [@jane.non_erasable.instances]
include M
