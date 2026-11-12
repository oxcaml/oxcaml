(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-no-flambda2-reaper -dcanonical-ids -dcmm";
 setup-ocamlopt.byte-build-env;
 compiler_output = "compiler-output.raw";
 ocamlopt.byte;
 script = "grep '^(function{' compiler-output.raw";
 output = "compiler-output";
 script;
 compiler_output = "compiler-output";
 check-ocamlopt.byte-output;
*)

(* Inlining [Make] copies [count], and inlining [C.step] in the copy lets
   Simplify unbox [r], so the copy is simplified twice.

   This checks the debuginfo of the copy of [count] in the function headers of
   the Cmm dump: it has the frame of [Make (...)] once, i.e.
   [function{resimplified_function_debuginfo.ml:43,...;resimplified_function_debuginfo.ml:32,...}].
   Do not promote a reference where that frame appears twice. *)

let[@inline never] side x = x + 1

module type Config = sig
  val limit : int

  val step : int ref -> int -> unit
end

module Make (C : Config) = struct
  let[@inline never] count n =
    let r = ref 0 in
    for i = 0 to n do
      C.step r i
    done;
    let s = !r in
    let[@inline never] inner a = if a < C.limit + s then side a else a in
    inner n + inner (n + 1)
end
[@@inline always]

module M = Make (struct
  let limit = 3

  let[@inline always] step r i = r := !r + i
end)
