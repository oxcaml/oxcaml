(* TEST
 arch64;
 not-macos;
 not-windows;
 flags = "-g";
 {
   reference = "${test_source_directory}/frametable_index.reference";
   native;
 }{
   flags = "-g -no-frametable-index";
   reference = "${test_source_directory}/frametable_index.no_index.reference";
   native;
 }
*)

(* The runtime finds frame descriptors through the index that ocamlopt stores
   in the executable after linking (see asmcomp/frame_index.ml), or, when
   linked with -no-frametable-index, through a hash table built at startup.
   The accompanying [.run] script checks the runtime's report and the section
   holding the index. The program itself exercises lookups: minor and major
   collections scan a deep stack holding live values, and backtraces walk it. *)

let () = Printexc.record_backtrace true

let rec build n acc =
  if n = 0
  then acc
  else begin
    let acc = string_of_int n :: acc in
    if n mod 1000 = 0 then Gc.minor ();
    build (n - 1) acc
  end

exception Deep of int

let rec raise_deep n = if n = 0 then raise (Deep n) else 1 + raise_deep (n - 1)

let () =
  let l = build 10_000 [] in
  Gc.full_major ();
  assert (List.length l = 10_000);
  (try ignore (raise_deep 100 : int) with Deep _ -> ());
  assert (Printexc.raw_backtrace_length (Printexc.get_raw_backtrace ()) > 0);
  assert (Printexc.raw_backtrace_length (Printexc.get_callstack 10) > 0)
