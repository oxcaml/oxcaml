external amd64 : unit -> bool @@ portable = "%arch_amd64"
external arm64 : unit -> bool @@ portable = "%arch_arm64"


let amd64 = amd64 ()
let arm64 = arm64 ()

let () =
  if amd64 then print_endline "amd64";
  if arm64 then print_endline "arm64";
  match Sys.arch with
  | Amd64 -> assert amd64
  | Arm64 -> assert arm64

(* Architecture specific intrinsics *)

(* Performance monitoring intrinsics *)
module Amd64 = struct
  external rdtsc : unit -> (int64[@unboxed]) = "caml_rdtsc" "caml_rdtsc_unboxed"
  [@@noalloc] [@@builtin]

end

module Arm64 = struct
  external read_cntvct_el0
    :  unit
    -> (int64[@unboxed])
    = "caml_arm64_read_cntvct_el0" "caml_arm64_read_cntvct_el0_unboxed"
  [@@noalloc][@@builtin]
end

external pause : unit -> unit = "caml_pause_hint" [@@noalloc] [@@builtin]

let[@inline always] cycle_counter () =
  match Sys.arch with
  | Amd64 -> Amd64.rdtsc ()
  | Arm64 -> Arm64.read_cntvct_el0 ()

let () =
  let t0 = cycle_counter () in
  pause ();
  let t1 = cycle_counter () in
  let same = Int64.equal t0 t1 in
  let zero = (Int64.equal t0 0L) in
  (* The check may be flaky on some systems. Any other use of this value
     would also be fine, or the assert can be replaced with ignore and opaque_identity
     if needed. *)
  assert (not same || not zero);
  ()
