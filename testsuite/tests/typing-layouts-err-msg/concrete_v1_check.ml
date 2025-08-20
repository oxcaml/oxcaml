(* TEST
 flags = "-extension layouts_alpha";
 toplevel;
*)

type t_void : void;;
external unbox_unit : unit -> t_void = "%unbox_unit";;

(unbox_unit () : t_void);;
