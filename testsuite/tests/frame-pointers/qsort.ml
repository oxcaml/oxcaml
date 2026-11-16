(* TEST
 modules = "qsort_.c";
 (* [-D_GNU_SOURCE] is needed when AddressSanitizer support is enabled in order
    for the [dlsym] workaround in [qsort_.c] to work. *)
 flags = "-ccopt -D_GNU_SOURCE";
 fp_variant_available;
 codegen_variant = "fp";
 {
   bytecode;
 }{
   native;
 }
*)

external with_frame : (unit -> 'a) -> 'a = "with_frame"
external check_frames : unit -> unit = "check_frames"

external in_callback : (unit -> 'a) -> 'a = "in_callback"
external in_callback_stk :
  int -> int -> int -> int -> int ->
  int -> int -> int -> int -> int ->
  (unit -> 'a) -> 'a = "in_callback_stk_byte" "in_callback_stk"

external sort2 : ('a -> 'a -> int) -> 'a -> 'a -> 'a * 'a = "sort2"

let rec recurse n =
  if n = 0 then 0 else 1 + recurse (n-1)

let f a b =
  check_frames ();
  let compared = ref false in
  let cmp_str a b =
    compared := true;
    let n = recurse 10000 in (* force stack realloc *)
    assert (n = 10000);
    (* check_frames not expected to work here:
       we're inside a call to qsort that may not have frame pointers *)
    Gc.minor ();
    String.compare a b
  in
  let a, b = sort2 cmp_str a b in
  check_frames ();
  (* Which element qsort passes first to the comparator (and how often it
     calls it) is up to the C library, so only check that the comparator
     ran and that qsort ordered the elements by its results. *)
  assert !compared;
  assert (String.compare a b <= 0);
  Printf.printf "Sorted: %s <= %s\n" a b

let in_finaliser f =
  let finalised = ref false in
  Gc.finalise_last (fun () -> finalised := true; f ()) (ref 42);
  Gc.minor ();
  assert (!finalised)

let () =
  in_callback @@ fun () ->
  with_frame @@ fun () ->
  in_finaliser @@ fun () ->
  in_callback @@ fun () ->
  in_callback_stk 10 10 10 10 10 10 10 10 10 10 (fun () ->
    f "foo" "bar";
    f "bar" "foo")
