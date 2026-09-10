(* TEST
 flags = "-extension layouts_beta -flambda2-reaper";
 flambda2;
 {
   native;
 }{
   ocamlopt_flags = "-O3";
   compiler_directory_suffix = ".O3";
   native;
 }{
   ocamlopt_flags = "-O3 -no-reaper-change-calling-conventions";
   compiler_directory_suffix = ".fixed-cc";
   native;
 }{
   ocamlopt_flags = "-O3 -no-flambda2-reaper";
   compiler_directory_suffix = ".no-reaper";
   native;
 }
*)

type (_ : any) witness =
  | Int : int witness
  | String : string witness

let[@inline never] poly : type (a : any). a witness -> unit -> a =
  fun w () -> match w with Int -> 42 | String -> "x"

let use_int () = poly Int ()

let use_string () = poly String ()

let () =
  assert (use_int () = 42);
  assert (String.equal (use_string ()) "x")

external box_float : float# -> float = "%box_float"

let[@inline never] forward : type (result : any).
    (unit -> result) -> result =
  fun callback -> callback ()

let[@inline never] return_int () = Sys.opaque_identity 42

let[@inline never] return_float () = #3.14

let[@inline never] return_product () = #(7, #3.14)

let[@inline never] run_local_forward (callback : unit -> float#) =
  let[@inline never] local_forward : type (result : any).
      int -> (unit -> result) -> result =
    fun _ callback -> callback ()
  in
  local_forward 0 callback

let () =
  assert (forward return_int = 42);
  assert (Float.equal (box_float (forward return_float)) 3.14);
  assert (Float.equal (box_float (run_local_forward return_float)) 3.14);
  let #(number, fraction) = forward return_product in
  assert (number = 7);
  assert (Float.equal (box_float fraction) 3.14)

type (_ : any) int_witness =
  | Int_result : int int_witness
  | Int_forward :
      ('result : any). (unit -> 'result) -> 'result int_witness

type (_ : any) float_witness =
  | Float_result : float# float_witness
  | Float_forward :
      ('result : any). (unit -> 'result) -> 'result float_witness

type (_ : any) product_witness =
  | Product_result : #(int * float#) product_witness
  | Product_forward :
      ('result : any). (unit -> 'result) -> 'result product_witness

let[@inline never] int_or_forward : type (result : any).
    result int_witness -> result = function
  | Int_result -> 42
  | Int_forward callback -> callback ()

let[@inline never] float_or_forward : type (result : any).
    result float_witness -> result = function
  | Float_result -> #3.14
  | Float_forward callback -> callback ()

let[@inline never] product_or_forward : type (result : any).
    result product_witness -> result = function
  | Product_result -> #(7, #3.14)
  | Product_forward callback -> callback ()

let () =
  assert (int_or_forward Int_result = 42);
  assert (Float.equal (box_float (int_or_forward (Int_forward return_float)))
            3.14);
  assert (Float.equal (box_float (float_or_forward Float_result)) 3.14);
  assert (float_or_forward (Float_forward return_int) = 42);
  assert (product_or_forward (Product_forward return_int) = 42);
  let #(number, fraction) = product_or_forward Product_result in
  assert (number = 7);
  assert (Float.equal (box_float fraction) 3.14);
  let #(number, fraction) =
    product_or_forward (Product_forward return_product)
  in
  assert (number = 7);
  assert (Float.equal (box_float fraction) 3.14)

let () =
  let[@inline never] local_int : type (result : any).
      result int_witness -> result = function
    | Int_result -> 42
    | Int_forward callback -> callback ()
  in
  assert (local_int Int_result = 42);
  assert (local_int (Int_forward return_int) = 42);
  assert (Float.equal (box_float (local_int (Int_forward return_float))) 3.14);
  let #(number, fraction) = local_int (Int_forward return_product) in
  assert (number = 7);
  assert (Float.equal (box_float fraction) 3.14)

let () =
  let[@inline never] local_float : type (result : any).
      result float_witness -> result = function
    | Float_result -> #3.14
    | Float_forward callback -> callback ()
  in
  assert (Float.equal (box_float (local_float Float_result)) 3.14);
  assert (local_float (Float_forward return_int) = 42);
  let #(number, fraction) = local_float (Float_forward return_product) in
  assert (number = 7);
  assert (Float.equal (box_float fraction) 3.14)

let () =
  let[@inline never] local_product : type (result : any).
      result product_witness -> result = function
    | Product_result -> #(7, #3.14)
    | Product_forward callback -> callback ()
  in
  assert (local_product (Product_forward return_int) = 42);
  let #(number, fraction) = local_product Product_result in
  assert (number = 7);
  assert (Float.equal (box_float fraction) 3.14);
  let #(number, fraction) = local_product (Product_forward return_product) in
  assert (number = 7);
  assert (Float.equal (box_float fraction) 3.14)
