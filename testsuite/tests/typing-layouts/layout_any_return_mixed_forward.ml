(* TEST
 flags = "-extension layouts_beta -flambda2-kind-checks";
 include stdlib_upstream_compatible;
 flambda2;
 {
   native;
 }
*)

(* A return-any function may mix a direct concrete return with a residual
   tail-forward to another unknown-result function.  The caller's result
   convention then comes from the forwarded callee on one path and from the
   direct return on the other. *)

module F = Stdlib_upstream_compatible.Float_u

type (_ : any) fwd_w = F_float : float# fwd_w

let[@inline never] forward : type (a : any). a fwd_w -> a =
  fun F_float -> #3.14

type (_ : any) w =
  | W_int : int w
  | W_fwd : ('a : any). 'a fwd_w -> 'a w

let[@inline never] f : type (a : any). a w -> a =
  fun w ->
    match w with
    | W_int -> 42
    | W_fwd fw -> forward fw

let[@inline never] use_int () = f W_int

let[@inline never] use_float () : float# = f (W_fwd F_float)

let () =
  assert (use_int () = 42);
  assert (Float.equal (F.to_float (use_float ())) 3.14)

type (_ : any) int_witness = Int : int int_witness

let[@inline never] forward_int : type (result : any).
  result int_witness -> result = fun Int -> 42

type (_ : any) reverse_witness =
  | Float : float# reverse_witness
  | Forward_int : ('result : any).
      'result int_witness -> 'result reverse_witness

let[@inline never] reverse : type (result : any).
  result reverse_witness -> result = function
  | Float -> #3.14
  | Forward_int witness -> forward_int witness

type (_ : any) product_witness =
  | Product : #(int * float#) product_witness
  | Product_forward : ('result : any).
      'result int_witness -> 'result product_witness

let[@inline never] product : type (result : any).
  result product_witness -> result = function
  | Product -> #(7, #3.14)
  | Product_forward witness -> forward_int witness

let () =
  assert (reverse (Forward_int Int) = 42);
  assert (Float.equal (F.to_float (reverse Float)) 3.14);
  assert (product (Product_forward Int) = 42);
  let #(number, fraction) = product Product in
  assert (number = 7);
  assert (Float.equal (F.to_float fraction) 3.14)
