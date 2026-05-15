(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

(* An all-void constructor is constant, so it accepts an explicit tag and
   counts toward the dense constant tag space. *)
type unit_u : void mod everything

[%%expect {|
type unit_u : void mod everything
|}]

external unbox_unit : unit -> unit_u = "%unbox_unit"

[%%expect {|
external unbox_unit : unit -> unit_u = "%unbox_unit"
|}]

type t =
  | A [@immediate 1]
  | B of unit_u [@immediate_all_void_constructor] [@immediate 0]

[%%expect {|
type t =
    A
  [@immediate 1]
  | B of unit_u [@immediate 0] [@immediate_all_void_constructor]
|}]

let tags = (Obj.magic A : int), (Obj.magic (B (unbox_unit ())) : int)

[%%expect {|
val tags : int * int = (1, 0)
|}]
