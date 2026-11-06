(* TEST
 flags = "-extension laws -laws-rocq rocq_state.v";
 setup-ocamlc.byte-build-env;
 module = "rocq_state.ml";
 ocamlc.byte;
 output = "${test_build_directory}/rocq_state.v";
 reference = "${test_source_directory}/rocq_state.v.reference";
 check-program-output;
*)

(* The Rocq file of stateful code: refs, arrays, sequencing and loops. *)

let rev_in_place a =
  let i = ref 0 and j = ref (Array.length a - 1) in
  while !i < !j do
    let t = a.(!i) in
    a.(!i) <- a.(!j);
    a.(!j) <- t;
    incr i;
    decr j
  done

let sum a =
  let s = ref 0 in
  for i = 0 to Array.length a - 1 do
    s := !s + Array.get a i
  done;
  !s

let fill_down a x =
  for i = Array.length a - 1 downto 0 do
    Array.set a i x
  done

let make_counter () =
  let c = { contents = 0 } in
  fun () ->
    c.contents <- c.contents + 1;
    c.contents

let count n =
  let c = ref 0 in
  for _ = 1 to n do
    incr c
  done;
  !c

let to_list_twice a = (Array.to_list a, Array.to_list (Array.make 2 0))
let get = Array.get
let length = Array.length

(* Stateful definitions that are not values are not translated: their
   effects would be repeated at each use. *)
let counter = make_counter ()

type t = { mutable x : int }

let bump r = r.x <- r.x + 1
let fail () = raise Exit
