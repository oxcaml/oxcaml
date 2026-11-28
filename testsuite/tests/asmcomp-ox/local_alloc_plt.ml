(* TEST
 modules = "local_alloc_plt_stubs.c";
 arch_amd64;
 linux;
 flags = "-O3 -regalloc irc -cclib -Wl,--wrap=caml_call_local_realloc";
 native;
*)

external opaque : ('a[@local_opt]) -> ('a[@local_opt]) = "%opaque"
external force_local_growth : unit -> unit = "force_local_growth" [@@noalloc]
external reserved_pair_written : unit -> bool = "reserved_pair_written"
  [@@noalloc]

(* Keep the arguments live across the allocation so its result can occupy a
   PLT-clobbered register. The linker wrapper clobbers r10 and r11 before the
   runtime helper saves them, independently of the system's PLT behaviour. *)
let[@inline never] alloc a b c d e f g h i j =
  let pair = opaque (stack_ (a, b)) in
  let x, y = pair in
  x + y + a + b + c + d + e + f + g + h + i + j

let () =
  force_local_growth ();
  let result =
    alloc (opaque 1) (opaque 2) (opaque 3) (opaque 4) (opaque 5)
      (opaque 6) (opaque 7) (opaque 8) (opaque 9) (opaque 10)
  in
  (* Checking the result alone is insufficient: writes and reads through the
     same corrupted pointer can still compute the right sum. *)
  assert (result = 58);
  assert (reserved_pair_written ())
