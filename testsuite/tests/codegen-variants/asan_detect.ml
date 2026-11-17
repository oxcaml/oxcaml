(* TEST
 codegen_variant = "asan-fp";
 variant_available;
 flags = "-I +unix";
 libraries = "unix";
 compiler_directory_suffix = ".asan-fp";
 native;
*)

(* A real out-of-bounds write detected by AddressSanitizer, in recover
   mode.  The one-byte store one past the end of a 16-byte char bigarray
   compiles to a direct instrumented store (no bounds check:
   [Array1.unsafe_set]), and the bigarray's data lives in a plain malloc'd
   buffer, so the store lands in the allocation's redzone and ASan reports
   a heap-buffer-overflow.  The runtime pins halt_on_error=false
   (__asan_default_options in runtime/memory.c) and the generated code
   calls the _noabort report wrappers, so execution continues after the
   report.

   The report text itself is full of addresses, PIDs and shadow-memory
   dumps, so instead of pinning it in the reference file the test captures
   stderr around the bad store and greps the capture for the two stable
   phrases. *)

let contains ~sub s =
  let n = String.length s and m = String.length sub in
  let rec loop i = i + m <= n && (String.sub s i m = sub || loop (i + 1)) in
  m = 0 || loop 0

let read_all path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let () =
  let big = Bigarray.Array1.create Bigarray.Char Bigarray.c_layout 16 in
  Bigarray.Array1.set big 0 'a';
  let log = "asan_detect.stderr" in
  let fd = Unix.openfile log [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600 in
  let saved_stderr = Unix.dup Unix.stderr in
  Unix.dup2 fd Unix.stderr;
  (* The out-of-bounds write: one byte past the 16-byte allocation. *)
  Bigarray.Array1.unsafe_set big 16 'x';
  Unix.dup2 saved_stderr Unix.stderr;
  Unix.close saved_stderr;
  Unix.close fd;
  let report = read_all log in
  if contains ~sub:"AddressSanitizer: heap-buffer-overflow" report
     && contains ~sub:"WRITE of size 1" report
  then print_endline "asan detected heap-buffer-overflow (WRITE of size 1)"
  else (
    print_endline "asan report missing or unexpected; captured stderr was:";
    print_string report);
  print_endline "execution continued after the report"
