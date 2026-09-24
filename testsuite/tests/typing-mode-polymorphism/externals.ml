(* TEST
 flags += "-extension mode_polymorphism_alpha -extension mode_polymorphism_printing -extension layout_poly_alpha";
 expect;
*)

external select
  : bool
  -> ('a[@local_opt]) @ [< 'm]
  -> ('a[@local_opt]) @ [< 'n]
  -> ('a[@local_opt]) @ [> 'm | 'n | dynamic]
  @@ stateless
  = "caml_csel_value"
[@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

[%%expect{|
external select :
  bool ->
  ('a [@local_opt]) @ [< 'n] ->
  ('a [@local_opt]) @ [< 'm] -> ('a [@local_opt]) @ [> 'm | 'n | dynamic]
  = "caml_csel_value" [@@no_coeffects] [@@no_effects] [@@builtin] [@@noalloc]
|}]

module Poly : sig
  val min
    :  'a @ [< 'm & global many]
    -> 'a @ [< 'n & global many]
    -> 'a @ [> 'm | 'n | global dynamic]
end = struct
  external ( <= ) : ('a[@local_opt]) @ 'm -> ('a[@local_opt]) @ 'n -> bool
    = "%lessequal"

  let min x y =
    let is_less = borrow_ x <= borrow_ y in
    select is_less x y [@exclave_if_local l ~reasons:[ May_return_regional ]]
  ;;
end

[%%expect{|
Line 1:
Error: The external function "caml_csel_value" is not available
|}]
