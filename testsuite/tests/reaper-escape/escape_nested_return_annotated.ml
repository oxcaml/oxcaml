(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_nested_return";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_nested_return] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
let[@inline never] get (local_ (o : int option)) = match o with Some v -> v | None -> 0

let[@inline never] sum_pair (local_ (p : int * int)) = fst p + snd p

module M : sig
  val g : int -> int
end = struct
  let[@inline never] mk x = exclave_ Some (Some x)

  let[@inline never] f x =
    exclave_ (match mk x with Some y -> y | None -> None)

  let[@inline never] g x =
    let y = f x in
    let local_ p = x + 100, x + 200 in
    get y + sum_pair p
end

let () = (Printf.printf [@inlined never]) "%d\n" (M.g 7)
