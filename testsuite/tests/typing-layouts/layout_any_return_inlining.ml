(* TEST
 flags = "-extension layouts_beta -no-flambda2-reaper -flambda2-kind-checks";
 flambda2;
 {
   native;
 }
*)

type (_ : any) witness =
  | Int : int witness
  | Direct_int : int witness
  | Let_int : int witness
  | Forward : ('result : any). (unit -> 'result) -> 'result witness

external box_float : float# -> float = "%box_float"

external raise_any : ('result : any). exn -> 'result = "%raise"

let[@inline never] return_int () = Sys.opaque_identity 42

let[@inline always] forward : type (result : any).
    (unit -> result) -> result =
  fun callback -> callback ()

let[@inline always] mixed : type (result : any). result witness -> result =
  function
  | Int -> 42
  | Direct_int -> return_int ()
  | Let_int -> let result = return_int () in result
  | Forward callback -> forward callback

let[@inline never] use_int (witness : int witness) = mixed witness

let[@inline never] use_float (witness : float# witness) : float# =
  mixed witness

let[@inline never] use_product (witness : #(int * int) witness)
    : #(int * int) =
  mixed witness

let[@inline never] use_product_try (witness : #(int * int) witness) initial =
  let saved = ref initial in
  try
    let #(first, second) = mixed witness in
    first + second + !saved
  with Not_found -> !saved

type (_ : any) float_witness =
  | Float : float# float_witness
  | Forward_float : ('result : any). 'result witness -> 'result float_witness

let[@inline always] mixed_float : type (result : any).
    result float_witness -> result = function
  | Float -> #3.14
  | Forward_float witness -> mixed witness

let[@inline never] use_mixed_float (witness : float# float_witness) : float# =
  mixed_float witness

let[@inline never] use_mixed_float_int (witness : int float_witness) =
  mixed_float witness

type (_ : any) product_witness =
  | Product : #(int * int) product_witness
  | Forward_product : ('result : any).
      'result float_witness -> 'result product_witness

let[@inline always] mixed_product : type (result : any).
    result product_witness -> result = function
  | Product -> #(21, 42)
  | Forward_product witness -> mixed_float witness

let[@inline never] use_mixed_product
    (witness : #(int * int) product_witness) : #(int * int) =
  mixed_product witness

let[@inline never] use_mixed_product_int (witness : int product_witness) =
  mixed_product witness

let () =
  assert (use_int Int = 42);
  assert (use_int Direct_int = 42);
  assert (use_int Let_int = 42);
  assert (use_int (Forward return_int) = 42);
  assert
    (Float.equal (box_float (use_float (Forward (fun () -> #3.14)))) 3.14);
  let #(first, second) = use_product (Forward (fun () -> #(21, 42))) in
  assert (first = 21 && second = 42);
  assert (use_product_try (Forward (fun () -> #(21, 42))) 7 = 70);
  assert (use_product_try (Forward (fun () -> raise_any Not_found)) 7 = 7);
  assert (Float.equal (box_float (use_mixed_float Float)) 3.14);
  assert (use_mixed_float_int (Forward_float Int) = 42);
  let #(first, second) = use_mixed_product Product in
  assert (first = 21 && second = 42);
  assert (use_mixed_product_int (Forward_product (Forward_float Int)) = 42)
