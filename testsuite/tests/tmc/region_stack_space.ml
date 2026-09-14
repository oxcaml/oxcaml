(* TEST
 stack-allocation;
 native;
*)

(* A [@tail_mod_cons] function that keeps a live local-allocation region
   silently loses its constant-stack guarantee.

   TMC still fires -- a dps variant is generated -- but its recursive call
   sits inside the region with [Rc_normal], so the region has to be closed
   after the call returns and the call stops being a tail call. Stack usage
   becomes O(n) instead of O(1), and nothing is reported at [-w +a].

   Any construct that keeps a region alive will do it. A [%probe] is one such
   construct, and an unavoidable one: the probe handler is applied in a
   local-returning mode, so every [%probe] in a [@tail_mod_cons] function
   costs it constant stack. Probes are native- and target-specific, so this
   test uses the two portable constructs instead.

   The bug is native-only: bytecode reports constant stack for all three.

   BUG: all three answers below should be "true". The last two are currently
   "false" and must be promoted when this is fixed. *)

let[@inline never] local_pair () = exclave_ (1, 2)

let[@inline never] use_local (local_ _x : int * int) = ()

(* No region: TMC works, stack stays constant. *)
let[@tail_mod_cons] rec map_plain t ~f =
  match t with
  | [] -> []
  | hd :: tl -> f hd :: map_plain tl ~f

(* A call to a local-returning function keeps a region alive. *)
let[@tail_mod_cons] rec map_exclave t ~f =
  let p = local_pair () in
  ignore (Sys.opaque_identity (fst p));
  match t with
  | [] -> []
  | hd :: tl -> f hd :: map_exclave tl ~f

(* So does a local allocation that actually escapes into a call. *)
let[@tail_mod_cons] rec map_local t ~f =
  let local_ p = (Sys.opaque_identity 1, Sys.opaque_identity 2) in
  use_local p;
  match t with
  | [] -> []
  | hd :: tl -> f hd :: map_local tl ~f

let depth () = Printexc.raw_backtrace_length (Printexc.get_callstack 10_000)

(* Tail-recursive, so building the input does not itself grow the stack. *)
let rec build acc n = if n = 0 then acc else build (n :: acc) (n - 1)

let large = 50

(* TMC guarantees [f] is applied to each element before the recursive call,
   so the samples come out in list order. The first element is mapped by the
   direct function before it delegates to the dps variant, so its depth
   differs either way; compare from the second element onwards.

   Only the shape is checked, never the absolute depths, which shift with the
   optimization level. *)
let report name map =
  let samples = ref [] in
  let sample i = samples := depth () :: !samples; i in
  ignore (map (build [] large) ~f:sample);
  match List.rev !samples with
  | [] | [_] -> Printf.printf "%-32s too few samples\n" name
  | _first :: (base :: _ as rest) ->
      let constant = List.for_all (fun d -> d = base) rest in
      Printf.printf "%-32s constant stack: %b\n" name constant

let () =
  report "no region:" map_plain;
  report "call to local-returning fn:" map_exclave;
  report "local_ allocation:" map_local
