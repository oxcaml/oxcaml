(* TEST
 flambda;
 ocamlopt_flags = "-flambda2-expert-cmm-safe-subst";
 exit_status = "2";
 native;
*)

module IArray = struct
  type 'a t = 'a iarray
  external length : local_ 'a iarray -> int @@ portable = "%array_length"
  external unsafe_get :
    ('a iarray[@local_opt]) -> int -> ('a[@local_opt]) @@ portable
    = "%array_unsafe_get"
  external unsafe_of_array : 'a array -> 'a iarray @@ portable
    = "%array_to_iarray"
end

(* Additional test case for Issue #4803 on the oxcaml repo. *)

let[@inline never] check_len arr i =
  if i >= 0 && i < IArray.length arr then i else assert false

let[@inline never][@local never] g (arr : int iarray) i =
  let j = check_len arr i in
  let x = IArray.unsafe_get arr i in
  (j, x + 1)

let _u =
  let arr = IArray.unsafe_of_array [| 1;2;3 |] in
  (* use an index big enough to cause a segfault if the program tries to access it *)
  g arr 1_000_000_000
