(* TEST
 stack-checks;
 native-compiler;
 no-tsan;
 no-address-sanitizer;
 linux;
 arch_amd64;
 script = "sh ${test_source_directory}/has_gdb.sh";
 config_script;
 setup-ocamlopt.byte-build-env;
 flags = "-g";
 ocamlopt.byte;
 debugger_script = "${test_source_directory}/gdb-script-stack-realloc";
 gdb;
 stdout = "";
 stderr = "";
 script = "sh ${test_source_directory}/sanitize.sh stack_realloc";
 script;
 check-program-output;
*)

(* The debugger prints the frames above four reallocations of the stack, each
   in a new fiber, and then above a raise. The frames of [entry], [branch] and
   [branches] are large enough for their stack checks, which come before their
   prologues, to fail there. With frame pointers, the debugger also checks the
   %rbp that it finds for the caller of the function at each stop. *)

let[@inline never] leaf x = x + 1

(* The check is the first instruction of the function. *)
let[@inline never] entry x =
  let a0 = Sys.opaque_identity (x + 0) in
  let a1 = Sys.opaque_identity (x + 1) in
  let a2 = Sys.opaque_identity (x + 2) in
  let a3 = Sys.opaque_identity (x + 3) in
  let a4 = Sys.opaque_identity (x + 4) in
  let a5 = Sys.opaque_identity (x + 5) in
  let a6 = Sys.opaque_identity (x + 6) in
  let a7 = Sys.opaque_identity (x + 7) in
  let a8 = Sys.opaque_identity (x + 8) in
  let a9 = Sys.opaque_identity (x + 9) in
  let a10 = Sys.opaque_identity (x + 10) in
  let a11 = Sys.opaque_identity (x + 11) in
  let a12 = Sys.opaque_identity (x + 12) in
  let a13 = Sys.opaque_identity (x + 13) in
  let a14 = Sys.opaque_identity (x + 14) in
  let a15 = Sys.opaque_identity (x + 15) in
  let a16 = Sys.opaque_identity (x + 16) in
  let a17 = Sys.opaque_identity (x + 17) in
  let a18 = Sys.opaque_identity (x + 18) in
  let a19 = Sys.opaque_identity (x + 19) in
  let a20 = Sys.opaque_identity (x + 20) in
  let a21 = Sys.opaque_identity (x + 21) in
  let a22 = Sys.opaque_identity (x + 22) in
  let a23 = Sys.opaque_identity (x + 23) in
  let a24 = Sys.opaque_identity (x + 24) in
  let a25 = Sys.opaque_identity (x + 25) in
  let a26 = Sys.opaque_identity (x + 26) in
  let a27 = Sys.opaque_identity (x + 27) in
  let a28 = Sys.opaque_identity (x + 28) in
  let a29 = Sys.opaque_identity (x + 29) in
  let a30 = Sys.opaque_identity (x + 30) in
  let a31 = Sys.opaque_identity (x + 31) in
  let a32 = Sys.opaque_identity (x + 32) in
  let a33 = Sys.opaque_identity (x + 33) in
  leaf x + a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12
  + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23 + a24
  + a25 + a26 + a27 + a28 + a29 + a30 + a31 + a32 + a33

let[@inline never] deref r = !r

(* The check and the prologue are those of the second branch, and come after
   the code of the first one, which has its own prologue. The third branch
   has no frame. *)
let[@inline never] branch n x =
  if n = 0
  then deref (ref x)
  else if n = 1
  then (
    let a0 = Sys.opaque_identity (x + 0) in
    let a1 = Sys.opaque_identity (x + 1) in
    let a2 = Sys.opaque_identity (x + 2) in
    let a3 = Sys.opaque_identity (x + 3) in
    let a4 = Sys.opaque_identity (x + 4) in
    let a5 = Sys.opaque_identity (x + 5) in
    let a6 = Sys.opaque_identity (x + 6) in
    let a7 = Sys.opaque_identity (x + 7) in
    let a8 = Sys.opaque_identity (x + 8) in
    let a9 = Sys.opaque_identity (x + 9) in
    let a10 = Sys.opaque_identity (x + 10) in
    let a11 = Sys.opaque_identity (x + 11) in
    let a12 = Sys.opaque_identity (x + 12) in
    let a13 = Sys.opaque_identity (x + 13) in
    let a14 = Sys.opaque_identity (x + 14) in
    let a15 = Sys.opaque_identity (x + 15) in
    let a16 = Sys.opaque_identity (x + 16) in
    let a17 = Sys.opaque_identity (x + 17) in
    let a18 = Sys.opaque_identity (x + 18) in
    let a19 = Sys.opaque_identity (x + 19) in
    let a20 = Sys.opaque_identity (x + 20) in
    let a21 = Sys.opaque_identity (x + 21) in
    let a22 = Sys.opaque_identity (x + 22) in
    let a23 = Sys.opaque_identity (x + 23) in
    let a24 = Sys.opaque_identity (x + 24) in
    let a25 = Sys.opaque_identity (x + 25) in
    let a26 = Sys.opaque_identity (x + 26) in
    let a27 = Sys.opaque_identity (x + 27) in
    let a28 = Sys.opaque_identity (x + 28) in
    leaf x + a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11
    + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23
    + a24 + a25 + a26 + a27 + a28)
  else leaf x

(* The check is common to the second and third branches, which have a
   prologue each: the fourth one has no frame. *)
let[@inline never] branches n x =
  if n = 0
  then deref (ref x)
  else if n = 1
  then leaf x + 1
  else if n = 2
  then (
    let a0 = Sys.opaque_identity (x + 0) in
    let a1 = Sys.opaque_identity (x + 1) in
    let a2 = Sys.opaque_identity (x + 2) in
    let a3 = Sys.opaque_identity (x + 3) in
    let a4 = Sys.opaque_identity (x + 4) in
    let a5 = Sys.opaque_identity (x + 5) in
    let a6 = Sys.opaque_identity (x + 6) in
    let a7 = Sys.opaque_identity (x + 7) in
    let a8 = Sys.opaque_identity (x + 8) in
    let a9 = Sys.opaque_identity (x + 9) in
    let a10 = Sys.opaque_identity (x + 10) in
    let a11 = Sys.opaque_identity (x + 11) in
    let a12 = Sys.opaque_identity (x + 12) in
    let a13 = Sys.opaque_identity (x + 13) in
    let a14 = Sys.opaque_identity (x + 14) in
    let a15 = Sys.opaque_identity (x + 15) in
    let a16 = Sys.opaque_identity (x + 16) in
    let a17 = Sys.opaque_identity (x + 17) in
    let a18 = Sys.opaque_identity (x + 18) in
    let a19 = Sys.opaque_identity (x + 19) in
    let a20 = Sys.opaque_identity (x + 20) in
    let a21 = Sys.opaque_identity (x + 21) in
    let a22 = Sys.opaque_identity (x + 22) in
    let a23 = Sys.opaque_identity (x + 23) in
    let a24 = Sys.opaque_identity (x + 24) in
    let a25 = Sys.opaque_identity (x + 25) in
    let a26 = Sys.opaque_identity (x + 26) in
    let a27 = Sys.opaque_identity (x + 27) in
    let a28 = Sys.opaque_identity (x + 28) in
    leaf x + a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11
    + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23
    + a24 + a25 + a26 + a27 + a28)
  else x

(* The check follows the prologue. *)
let[@inline never] rec deep n = if leaf n > 1 then deep (n - 1) + 1 else 0

(* The block that follows the raise has no frame. *)
let[@inline never] raises n x =
  match n with
  | 0 -> x + 1
  | 1 -> raise Exit
  | _ -> x + 3

let[@inline never] calls_entry x = entry x + 1

let[@inline never] calls_branch x = branch (Sys.opaque_identity 1) x + 1

let[@inline never] calls_branches x = branches (Sys.opaque_identity 1) x + 1

let[@inline never] calls_raises x = raises (Sys.opaque_identity 1) x + 1

let in_fiber f x =
  ignore
    (Effect.Deep.match_with f x
       { retc = Fun.id; exnc = raise; effc = (fun _ -> None) }
      : int)

let () =
  in_fiber calls_entry 1;
  in_fiber calls_branch 1;
  in_fiber calls_branches 1;
  in_fiber deep 30;
  Printexc.record_backtrace true;
  match calls_raises 1 with
  | (_ : int) -> ()
  | exception Exit -> ()
