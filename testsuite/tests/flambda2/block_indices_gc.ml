(* TEST
 flambda2;
 native;
*)

external get64 : 'a -> int64_u -> int64_u = "%get_idx"
external set64 : 'a -> int64_u -> int64_u -> unit = "%set_idx"
external eq : int64_u -> int64_u -> bool = "%int64#_equal"
external add : int64_u -> int64_u -> int64_u = "%int64#_add"

type block = { mutable x : int64_u; mutable y : int64_u }

let[@inline never] read b i =
  let y = get64 b (add i #0x8000_0008L) in
  let r = ref b in
  let x = get64 b (add i #0x8000_0000L) in
  #(x, y, r)


let[@inline never] write b i =
  set64 b (add i #0x8000_0000L) #41L;
  let r = ref 0 in
  set64 b (add i #0x8000_0008L) #81L;
  r

let () = Gc.set { (Gc.get ()) with minor_heap_size = 4096 }

let () =
  Gc.minor ();
  for _ = 1 to 15 do ignore (Sys.opaque_identity (Array.make 256 0)) done;
  ignore (Sys.opaque_identity (Array.make 237 0));
  let b = { x = #42L; y = #84L } in
  (* At this point, the minor heap is full *)
  let #(x, _, _) = read b (-#0x8000_0000L) in
  if not (eq x #42L)
  then print_endline "Read error"

let () =
  Gc.minor ();
  for _ = 1 to 15 do ignore (Sys.opaque_identity (Array.make 256 0)) done;
  ignore (Sys.opaque_identity (Array.make 237 0));
  let b = { x = #42L; y = #84L } in
  (* At this point, the minor heap is full *)
  let _ = write b (-#0x8000_0000L) in
  if not (eq b.y #81L)
  then print_endline "Write error"
