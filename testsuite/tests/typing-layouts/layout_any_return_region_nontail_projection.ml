(* TEST
 flags = "-extension layouts_beta -extension mode";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

(* Field projection is a concrete value-shaped normal exit, so it may join
   another value exit in a single direct-return layout even under a local
   region. *)

external opaque_local : local_ 'a -> local_ 'a = "%opaque"

type boxed_record = { field : int }

type (_ : any) project_w =
  | Project_field : boxed_record -> int project_w
  | Project_str : string project_w

let[@inline never] concrete_projection : type (a : any). a project_w -> a =
  fun w ->
    let local_ x = ref 0 in
    let local_ _y = opaque_local x in
    match w with
    | Project_field r -> r.field
    | Project_str -> "s"

type projection_calls =
  { call_field : unit -> int;
    call_str : unit -> string
  }

let[@inline never] make_projection_calls
    (f : ('a : any). 'a project_w -> 'a) =
  { call_field = (fun () -> f (Project_field { field = 7 }));
    call_str = (fun () -> f Project_str)
  }

let () =
  assert (concrete_projection (Project_field { field = 7 }) = 7);
  assert (String.equal (concrete_projection Project_str) "s");
  let calls = make_projection_calls concrete_projection in
  assert (calls.call_field () = 7);
  assert (String.equal (calls.call_str ()) "s");
  ()
