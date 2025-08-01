(* TEST
 include stdlib_upstream_compatible;
  modules = "stubs.c";
  native;
*)

module Int64_u = Stdlib_upstream_compatible.Int64_u

external get_malloc_bytes : unit -> int = "get_malloc_bytes" "get_malloc_bytes"

let test_with_malloc_tracking name f =
  let before = get_malloc_bytes () in
  f ();
  let after = get_malloc_bytes () in
  Printf.printf "%s: Allocation Delta: %d\n" name (after - before)

let g x y = malloc_ (x,y)
let () =
  let m = g 2 3 in
  test_with_malloc_tracking "tuple" (fun () ->
    let #(a,b) = free_ m in
    Printf.printf "%d %d\n" a b)

type t1 = {x : int}
let g1 x = malloc_ {x}
let () =
  let m = g1 2 in
  test_with_malloc_tracking "t1" (fun () ->
    let #{x} = free_ m in
    Printf.printf "%d\n" x)

type t2 = {x : int; y : float}
let g2 x y = malloc_ {x;y}
let () =
  let m = g2 2 3.14 in
  test_with_malloc_tracking "t2" (fun () ->
    let #{x;y} = free_ m in
    Printf.printf "%d %g\n" x y)

type t3 = {x : int; y : int64#}
let g3 x y = malloc_ {x;y}
let () =
  let m = g3 2 (Int64_u.of_int 3) in
  test_with_malloc_tracking "t3" (fun () ->
    let #{x;y} = free_ m in
    Printf.printf "%d %Ld\n" x (Int64_u.to_int64 y))

(* type t4 = {x : int64#; y : int}
let g4 x y = malloc_ {x;y}
let () =
  let m = g4 (Int64_u.of_int 2) 3 in
  test_with_malloc_tracking "t4" (fun () ->
    let #{x;y} = free_ m in
    Printf.printf "%Ld %d\n" (Int64_u.to_int64 x) y) *)
