(* TEST
   modules = "specialise_lifted_mutual_product_lib.ml";
   flambda2;
   flags += "-O4 -flambda2-reaper -reaper-local-fields";
   { native; }
 *)

let[@zero_alloc] alternate n =
  Specialise_lifted_mutual_product_lib.mutual
    (fun x -> x * x) (fun x -> 2 * x + 1) n

let[@zero_alloc] sum_products a b n =
  Specialise_lifted_mutual_product_lib.product
    (fun #(a, b) -> 10 * a + b) #(a, b) n

let () =
  Printf.printf "%d %d %d\n" (alternate 0) (alternate 1) (alternate 4);
  Printf.printf "%d %d %d\n"
    (sum_products 2 3 0) (sum_products 2 3 3) (sum_products (-1) 4 2)
