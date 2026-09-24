(* TEST
 flambda2;
 stack-allocation;
 native;
*)

(* The runtime functions that create strings with uninitialized contents are
   called for unknown lengths, while small strings of known length are allocated
   inline. For local strings, the [@zero_alloc] annotations below check that no
   runtime function is called. Both must create the same strings. *)

external repr : ('a[@local_opt]) -> (Obj.t[@local_opt]) = "%identity"
external is_stack : Obj.t @ local -> bool = "caml_obj_is_stack"
external tag : Obj.t @ local -> int = "caml_obj_tag" [@@noalloc]
external size : Obj.t @ local -> int = "%obj_size"
external local_stack_offset : unit -> int = "caml_local_stack_offset"

(* The C API functions take untagged lengths. *)
external alloc_string : (int[@untagged]) -> bytes
  = "caml_no_bytecode_impl" "caml_alloc_string"

external alloc_local_string : (int[@untagged]) -> bytes @ local
  = "caml_no_bytecode_impl" "caml_alloc_local_string"

let bytes_per_word = Sys.word_size / 8

let local_stack_words () = local_stack_offset () / bytes_per_word

(* Fill the memory that the next allocation reuses, on the local stack or the
   minor heap, with ['\xff'], so that the checks below notice any padding that
   is left uninitialized. These strings are at least as long as the ones checked
   below, apart from those too long for the minor heap. *)
let[@inline never] dirty_memory () =
  let local = Bytes.create__stack (Sys.opaque_identity 4095) in
  Bytes.unsafe_fill local 0 4095 '\xff';
  Gc.minor ();
  ignore (Sys.opaque_identity (Bytes.make 2047 '\xff'));
  Gc.minor ()

let[@inline never] describe ~(make : unit -> bytes @ local) =
  dirty_memory ();
  let before = local_stack_words () in
  let b = make () in
  let words = local_stack_words () - before in
  Gc.full_major ();
  let length = Bytes.length b in
  let padding = Buffer.create bytes_per_word in
  for i = length to (size (repr b) * bytes_per_word) - 1 do
    Buffer.add_char padding (Bytes.unsafe_get b i)
  done;
  Printf.sprintf
    "Bytes.length %d, tag %d, size %d, padding %S, words %d, stack %b" length
    (tag (repr b)) (size (repr b)) (Buffer.contents padding) words
    (is_stack (repr b))

let describe_with_unknown_length ~(make : int -> bytes @ local) n =
  describe ~make:(fun () -> exclave_ make (Sys.opaque_identity n))

let () =
  List.iter
    (fun (name, make) ->
      List.iter
        (fun n ->
          Printf.printf "%s, length %d: %s\n" name n
            (describe_with_unknown_length ~make n))
        [0; 1; 5; 7; 8; 9; 2047; 2048])
    [ "Bytes.create__stack", Bytes.create__stack;
      "Bytes.create", fun n -> Bytes.create n ]

let[@inline never] check_known_lengths name ~make
    ~(known_lengths : (int * (unit -> bytes @ local)) list) =
  List.iter
    (fun (n, make_with_known_length) ->
      let actual = describe ~make:make_with_known_length in
      let expected = describe_with_unknown_length ~make n in
      if not (String.equal actual expected)
      then
        Printf.printf "%s, known length %d: %s, but %s for unknown length\n"
          name n actual expected)
    known_lengths;
  Printf.printf "%s: checked known lengths %s\n" name
    (String.concat ", "
       (List.map (fun (n, _) -> Int.to_string n) known_lengths))

(* The largest known length is that of strings of [Config.max_young_wosize]
   words, which are the largest strings allocated inline. *)
let () =
  check_known_lengths "Bytes.create__stack" ~make:Bytes.create__stack
    ~known_lengths:
      [ 0, (fun[@zero_alloc] () -> exclave_ Bytes.create__stack 0);
        1, (fun[@zero_alloc] () -> exclave_ Bytes.create__stack 1);
        2, (fun[@zero_alloc] () -> exclave_ Bytes.create__stack 2);
        3, (fun[@zero_alloc] () -> exclave_ Bytes.create__stack 3);
        4, (fun[@zero_alloc] () -> exclave_ Bytes.create__stack 4);
        5, (fun[@zero_alloc] () -> exclave_ Bytes.create__stack 5);
        6, (fun[@zero_alloc] () -> exclave_ Bytes.create__stack 6);
        7, (fun[@zero_alloc] () -> exclave_ Bytes.create__stack 7);
        8, (fun[@zero_alloc] () -> exclave_ Bytes.create__stack 8);
        9, (fun[@zero_alloc] () -> exclave_ Bytes.create__stack 9);
        2047, (fun[@zero_alloc] () -> exclave_ Bytes.create__stack 2047) ];
  check_known_lengths "Bytes.create" ~make:(fun n -> Bytes.create n)
    ~known_lengths:
      [ 0, (fun () -> Bytes.create 0);
        5, (fun () -> Bytes.create 5);
        8, (fun () -> Bytes.create 8);
        2047, (fun () -> Bytes.create 2047) ];
  check_known_lengths "caml_alloc_local_string" ~make:alloc_local_string
    ~known_lengths:
      [ 0, (fun[@zero_alloc] () -> exclave_ alloc_local_string 0);
        5, (fun[@zero_alloc] () -> exclave_ alloc_local_string 5);
        2047, (fun[@zero_alloc] () -> exclave_ alloc_local_string 2047) ];
  check_known_lengths "caml_alloc_string" ~make:(fun n -> alloc_string n)
    ~known_lengths:
      [ 0, (fun () -> alloc_string 0);
        5, (fun () -> alloc_string 5);
        2047, (fun () -> alloc_string 2047) ]

(* The runtime raises the exception for invalid lengths, even known ones. *)
let () =
  let check name (make : unit -> bytes @ local) =
    match make () with
    | (_ : bytes) -> Printf.printf "%s: no exception\n" name
    | exception Invalid_argument msg ->
      Printf.printf "%s: Invalid_argument %S\n" name msg
  in
  check "Bytes.create, known length -1" (fun () -> Bytes.create (-1));
  check "Bytes.create, known length max_int" (fun () -> Bytes.create max_int);
  check "Bytes.create__stack, known length -1" (fun () ->
      exclave_ Bytes.create__stack (-1));
  check "Bytes.create__stack, known length min_int" (fun () ->
      exclave_ Bytes.create__stack min_int)

(* Bounds checks against known lengths that must fail still raise. *)
let () =
  let b = Bytes.create 5 in
  Bytes.set b 4 'a';
  match Bytes.set b 5 'a' with
  | () -> print_endline "Bytes.set, index 5 of known length 5: no exception"
  | exception Invalid_argument msg ->
    Printf.printf "Bytes.set, index 5 of known length 5: Invalid_argument %S\n"
      msg

(* The contents of strings created by the runtime may change, so [%obj_dup]
   must copy them. *)
external dup : bytes -> bytes = "%obj_dup"

let () =
  let b = Bytes.create 5 in
  Bytes.fill b 0 5 'a';
  let copy = dup b in
  Bytes.set copy 0 'b';
  Printf.printf "%%obj_dup: original %S, copy %S\n" (Bytes.to_string b)
    (Bytes.to_string copy)
