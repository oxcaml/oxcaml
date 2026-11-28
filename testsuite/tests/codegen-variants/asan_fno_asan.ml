(* TEST
 codegen_variant = "+asan";
 variant_available;
 readonly_files = "asan_fno_asan_unit.ml";
 setup-ocamlopt.byte-build-env;
 flags = "-fno-asan";
 module = "asan_fno_asan_unit.ml";
 ocamlopt.byte;
 flags = "-I +unix";
 libraries = "unix";
 module = "";
 binary_modules = "asan_fno_asan_unit";
 ocamlopt.byte;
 run;
 check-program-output;
*)

(* -fno-asan leaves a unit uninstrumented without changing its codegen
   variant: Asan_fno_asan_unit is compiled with it, links into this
   AddressSanitizer program (a unit of another variant would be rejected,
   see asan_mismatch.ml) and runs.  Both units perform the same store one
   byte past the end of a malloc'd buffer (see asan_detect.ml): the
   uninstrumented one goes unnoticed, the instrumented one is reported. *)

let[@inline never] write_past_end (buffer : Asan_fno_asan_unit.buffer) =
  Bigarray.Array1.unsafe_set buffer (Bigarray.Array1.dim buffer) 'x'

let contains ~sub s =
  let n = String.length s and m = String.length sub in
  let rec loop i = i + m <= n && (String.sub s i m = sub || loop (i + 1)) in
  m = 0 || loop 0

let read_all path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let reports_overflow write =
  let buffer = Bigarray.Array1.create Bigarray.Char Bigarray.c_layout 16 in
  let log = "asan_fno_asan.stderr" in
  let fd =
    Unix.openfile log [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600
  in
  let saved_stderr = Unix.dup Unix.stderr in
  Unix.dup2 fd Unix.stderr;
  write buffer;
  Unix.dup2 saved_stderr Unix.stderr;
  Unix.close saved_stderr;
  Unix.close fd;
  contains ~sub:"AddressSanitizer: heap-buffer-overflow" (read_all log)

let () =
  Printf.printf "unit compiled with -fno-asan: reported %b\n"
    (reports_overflow Asan_fno_asan_unit.write_past_end);
  Printf.printf "instrumented unit: reported %b\n"
    (reports_overflow write_past_end)
