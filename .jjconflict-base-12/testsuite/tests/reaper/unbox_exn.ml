(* TEST
   flambda2;
   flags += "-flambda2-reaper";
   { native; }		
 *)		
		
[@@@warning "-ignored-extra-argument"]

external __dummy2__ : unit -> 'a = "%opaque"
external raise : exn -> 'a = "%raise"

exception Found_int of int 
let substr_eq ?start:_  _ ~pattern:_ = __dummy2__ ()[@@inline never ]
  [@@local never ]
let find_from ?(start= 0) str ~pattern =
  try
    for i = 0 to 0 do
      if
        substr_eq ?start:(__dummy2__ ()) (__dummy2__ ())
          ~pattern:(__dummy2__ ())
      then raise (Found_int i)
    done;
    __dummy2__ ()
  with | Found_int i -> Some i | _ -> __dummy2__ ()[@@inline never ][@@local never
                                                                    ]