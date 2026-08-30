(* A [%probe] inside a [@@tail_mod_cons] function silently costs TMC its
   constant-stack guarantee.

   The probe handler is applied in a local-returning mode (see [Texp_probe] in
   translcore.ml, "we conservatively assume that all arguments are local"), so
   the enclosing function keeps a region. TMC still fires -- a [map_dps] variant
   is generated -- but its recursive call sits inside that region with
   [Rc_normal], so the region has to be closed after the call returns and the
   call stops being a tail call. Stack usage becomes O(n) instead of O(1), and
   nothing is reported at [-w +a].

   Rather than exhausting the stack, this test samples the call-stack depth at
   each element and reports the shape of the growth. Mapping 50 elements on
   aarch64-darwin, the sampled depths run 6, 6, 6, ... 6 without a probe, and 6,
   7, 8, ... 54 with one. Only the shape is asserted, never the absolute values,
   which shift with the optimization level.

   BUG: the expected output records the second shape. When TMC and probes are
   fixed the two answers swap and tmc.expected must be updated. *)

module X = struct
  let side_effect = ()
end

let depth () = Printexc.raw_backtrace_length (Printexc.get_callstack 10_000)

let[@tail_mod_cons] rec map t ~f =
  match
    [%probe "x" X.side_effect];
    t
  with
  | [] ->
    [%probe "x" X.side_effect];
    []
  | hd :: tl ->
    f
      ([%probe "x" X.side_effect];
       hd)
    :: map tl ~f

(* Tail-recursive, so building the input does not itself grow the stack. *)
let rec build acc n = if n = 0 then acc else build (n :: acc) (n - 1)

let large = 50

let () =
  let samples = ref [] in
  (* TMC guarantees [f] is applied to each element before the recursive call, so
     the samples come out in list order. *)
  ignore
    (map (build [] large) ~f:(fun i ->
         samples := depth () :: !samples;
         i));
  (* The first element is mapped by the direct function before it delegates to
     the destination-passing variant, so its depth differs either way. Compare
     from the second element onwards. *)
  match List.rev !samples with
  | [] | [_] -> print_endline "too few samples"
  | _first :: (base :: _ as rest) ->
    let constant = List.for_all (fun d -> d = base) rest in
    let grows_by_one_per_element =
      List.for_all
        (fun (i, d) -> d = base + i)
        (List.mapi (fun i d -> i, d) rest)
    in
    Printf.printf "stack depth constant from element 2 onwards: %b\n" constant;
    Printf.printf "stack depth grows one frame per element:     %b\n"
      grows_by_one_per_element
