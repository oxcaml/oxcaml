(* End-to-end FDO test of a switch compiled to a jump table (see dune): the
   edges of the indirect jump are profiled like those of conditional branches.
   Value [k] occurs [k + 1] times, so the arms' counts are 1 to 8. *)

external trace : append:bool -> string -> (unit -> 'a) -> 'a
  = "caml_singlestep_trace"

let[@inline never] step x acc =
  match x with
  | 0 -> acc + 1
  | 1 -> acc * 3
  | 2 -> acc - 7
  | 3 -> acc lxor 5
  | 4 -> acc + x
  | 5 -> acc lsl 1
  | 6 -> acc - x
  | _ -> acc lor 9

let[@inline never] run a =
  let acc = ref 0 in
  for i = 0 to Array.length a - 1 do
    acc := step (Array.unsafe_get a i) !acc
  done;
  !acc

let () =
  let a = Array.concat (List.init 8 (fun k -> Array.make (k + 1) k)) in
  let result = trace ~append:false Sys.argv.(1) (fun () -> run a) in
  Printf.printf "%d\n" result
