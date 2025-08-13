let[@inline never] [@local never] rec my_fold ~f ~init (local_ xs) = 
  match xs with
  | [] -> init
  | x :: xs -> f x (my_fold ~f ~init xs)

let[@inline never] [@local never] rec local_iota ~start n = exclave_
  if n <= 0 then [] 
  else start :: local_iota ~start:(start+1) (n-1)

let[@inline never] [@local never] big_local_alloc () =
  let xs = local_  local_iota ~start:0 10000 in
  my_fold ~f:(+) ~init:0 xs [@nontail]

let[@inline never] [@local never] rec heap_iota ~start n =
  if n <= 0 then [] 
  else start :: heap_iota ~start:(start+1) (n-1)

let[@inline never] [@local never] big_heap_alloc () =
  let xs = heap_iota ~start:0 20000 in
  List.fold_left (+) 0 xs

let[@inline never] [@local never] make_ref () = ref 0

let heap_ref_incr () =
  let r = make_ref () in
  incr r; incr r; incr r;
  !r

let () =
  Format.printf "big_local_alloc: %d\n" (big_local_alloc ());
  Format.printf "big_heap_alloc: %d\n" (big_heap_alloc ());
  Format.printf "heap_ref_incr: %d\n" (heap_ref_incr ());

