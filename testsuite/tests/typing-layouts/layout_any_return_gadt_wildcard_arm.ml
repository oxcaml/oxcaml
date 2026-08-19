(* TEST
 flags = "-extension layouts_beta";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

(* A GADT function can have a return type of jkind [any] while every
   concrete arm returns the same sort.  The return type itself is not
   representable, so the function keeps the unknown-result return
   convention; the try-with joins and region allocations inside the
   arms still get concrete layouts from the sorts recorded in the
   typedtree. *)

type (_ : any) w =
  | WI : int w
  | WF : float# w

let try_leaf : type (a : any). a w -> a = function
  | WI -> (try 4 with _ -> 0)
  | _ -> assert false

let region_leaf : type (a : any). a w -> a = function
  | WI -> !(ref 3)
  | _ -> assert false

let try_region_leaf : type (a : any). a w -> a = function
  | WI -> (try !(ref (Sys.opaque_identity 7)) with _ -> 0)
  | _ -> assert false

let () =
  assert (try_leaf WI = 4);
  assert (region_leaf WI = 3);
  assert (try_region_leaf WI = 7)
