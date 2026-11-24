(* TEST *)
let[@inline never] f (g : _ @ local -> unit) n =
  let r = stack_ { contents = ref 0 } in
  g r;
  r.contents <- ref n;
  Gc.minor ();
  r.contents.contents

let _ =
  Printf.printf "%d\n" (f (fun _ -> ()) 42)
