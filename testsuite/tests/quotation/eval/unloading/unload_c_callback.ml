(* TEST
  modules = "unload_c_callback_.c";
  include eval;
  flags = "-extension runtime_metaprogramming";
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* H.2: the eval'd worker is reached through a C trampoline, and triggers
   a Gc.compact inside a callback to host code. The OCaml stack contains
   a C-OCaml boundary between the host main frame and the eval'd worker
   frame; F.2 must still walk the upper OCaml chunk to find the eval'd
   unit's return address and darken its Code_block. *)

external c_trampoline : ('a -> int -> unit -> int) -> 'a -> int -> int
  = "c_trampoline"

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n%!"
    label r u (r - u)

let worker = ref (fun _cb _n () -> 0)
let callback_fired = ref false

let[@inline never] host_callback () =
  callback_fired := true;
  worker := (fun _ _ () -> 0);
  Gc.compact ();
  Gc.compact ()

let[@inline never] populate () =
  worker := Eval.eval <[
    fun (cb : unit -> unit) (n : int) () ->
      let acc = ref 0 in
      for i = 1 to n do
        acc := !acc + i
      done;
      cb ();
      for i = 1 to n do
        acc := !acc + i
      done;
      !acc
  ]>

let () =
  report "start";
  populate ();
  let r = c_trampoline !worker host_callback 10 in
  Printf.printf "via c_trampoline: worker 10 returned %d (expected 110)\n" r;
  Printf.printf "callback_fired = %b\n" !callback_fired;
  report "after run";
  Gc.compact ();
  report "after final Gc.compact"
