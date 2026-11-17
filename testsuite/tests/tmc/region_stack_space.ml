(* TEST
 stack-allocation;
 native;
*)


let[@inline never] local_pair () = exclave_ (1, 2)

let[@inline never] use_local (local_ _x : int * int) = ()

(* tail recursive, no region. Stack space constant *)
let rec rev_map acc xs ~f =
  match xs with
  | [] -> acc
  | x::xs -> rev_map ((f x)::acc) xs ~f

(* tail recursive, with a region. Stack space constant *)
let rec rev_map_with_local acc xs ~f =
  let p = local_pair () in
  ignore (Sys.opaque_identity (fst p));
  match xs with
  | [] -> acc
  | x::xs -> rev_map_with_local (f x::acc) xs ~f

(* TMC, no region, Stack space constant *)
let[@tail_mod_cons] rec map_plain t ~f =
  match t with
  | [] -> []
  | hd :: tl -> f hd :: map_plain tl ~f


(* TMC, with region, no local parameters, Stack space constant *)
let[@tail_mod_cons] rec map_exclave t ~f =
  let p = local_pair () in
  ignore (Sys.opaque_identity (fst p));
  match t with
  | [] -> []
  | hd :: tl -> f hd :: map_exclave tl ~f

(* TMC, with region, no local parameters, does a local allocation that actually escapes into a call. *)
let[@tail_mod_cons] rec map_local t ~f =
  let local_ p = (Sys.opaque_identity 1, Sys.opaque_identity 2) in
  use_local p;
  match t with
  | [] -> []
  | hd :: tl -> f hd :: map_local tl ~f

(* TMC, regions + parameters at local. The recursive call only passes
 * parameters and fields of parameters, so the region can still be closed.
 * Stack space constant.
 * *)
let[@tail_mod_cons] rec map_local_param (bound : (int * int) @ local) t ~f =
  let p = local_pair () in
  ignore (Sys.opaque_identity (fst p));
  use_local bound;
  match t with
  | [] -> []
  | hd :: tl -> f hd :: map_local_param bound tl ~f

(* TMC, regions + the recursive call uses the data we create on each local
   stack frame. This fundamentally can't be constant stack space: the TMC pass
   cannot close the region, and warns. *)
let[@tail_mod_cons] rec map_using_local t ~f =
  let f' = stack_ (fun x -> (f x) + 1) in
  match t with
  | [] -> []
  | hd :: tl -> f' hd :: map_using_local tl ~f:f'

(* TMC, multiple regions. Constant stack space *)
let[@tail_mod_cons] rec map_nested_regions t ~f =
  let p = local_pair () in
  ignore (Sys.opaque_identity (fst p));
  let go t =
    let q = local_pair () in
    ignore (Sys.opaque_identity (fst q));
    match t with
    | [] -> []
    | hd :: tl -> f hd :: map_nested_regions tl ~f
  in
  go t

(* TMC, multiple regions. Constant stack space *)
let[@tail_mod_cons] rec map_nested_regions_shared t ~f =
  let p = local_pair () in
  ignore (Sys.opaque_identity (fst p));
  let go t =
    let q = local_pair () in
    ignore (Sys.opaque_identity (fst q));
    match t with
    | [] -> []
    | hd :: tl -> f hd :: map_nested_regions_shared tl ~f
  in
  if Sys.opaque_identity true then go t else go (List.rev t)

(* TMC through mutually recursive helper:
 * Region appears because we can infer the option is at local
 *)
let[@tail_mod_cons] rec map_via_helper t ~f =
  match t with
  | [] -> []
  | hd :: tl ->
      let first =
        match Sys.opaque_identity (Some hd), Sys.opaque_identity (Some 0) with
        | Some x, Some y -> Some (min x y)
        | None, x | x, None -> x
      in
      let repeat = match first with None -> 0 | Some _ -> 1 in
      f hd :: helper ~repeat tl ~f
and[@tail_mod_cons] helper ~repeat t ~f =
  if repeat = 0 then map_via_helper t ~f
  else 0 :: helper ~repeat:(repeat - 1) t ~f

(* TMC, region + parameters at local, but every argument of the recursive call
   is a parameter ([ctx], [f]), a field of a parameter ([tl], out of the
   local [t]), an immediate ([count + 1]) or a heap allocation
   ([count :: seen]), none of which can point into the region.
   Stack space constant. *)
let last_seen = ref []

let[@tail_mod_cons] rec map_local_args (local_ ctx) ~count ~seen (local_ t)
    ~f =
  let p = local_pair () in
  ignore (Sys.opaque_identity (fst p));
  use_local ctx;
  match t with
  | [] -> last_seen := seen; ignore (Sys.opaque_identity count); []
  | hd :: tl ->
    f hd :: map_local_args ctx ~count:(count + 1) ~seen:(count :: seen) tl ~f

(* TMC, region + a parameter at local. The direct tail call on even elements
   is passed the result of a call, which the TMC pass cannot classify, but
   the type checker already allowed that call to close the region, and the
   dps version keeps it. Stack space constant. *)
let[@inline never] fresh n = (n, n)

let[@tail_mod_cons] rec map_tail_call (local_ prev : int * int) t ~f =
  let p = local_pair () in
  ignore (Sys.opaque_identity (fst p));
  use_local prev;
  match t with
  | [] -> []
  | hd :: tl when hd mod 2 = 0 -> map_tail_call (fresh hd) tl ~f
  | hd :: tl -> f hd :: map_tail_call prev tl ~f

(* TMC, region + a parameter at local. The recursive call is passed values
   that cannot point into the region: the result of a call returning at the
   global mode, a module global and an immediate. Stack space constant. *)
let[@inline never] next (local_ n : int ref) = !n + 1

let[@tail_mod_cons] rec map_global_args (local_ prev : int * int)
    (local_ cmp : int -> int -> int) (local_ k : int) t ~f =
  let p = local_pair () in
  ignore (Sys.opaque_identity (fst p));
  use_local prev;
  match t with
  | [] -> []
  | hd :: tl ->
      let r = stack_ (ref k) in
      f (hd + cmp k k)
      :: map_global_args (fresh hd) Int.compare (next r) tl ~f

(* TMC, region + a heap-allocated helper closure used twice, so that it is
   not inlined. Stack space constant. *)
type tree = Leaf | Node of int * tree * tree

let[@tail_mod_cons] rec map_global_closure t ~f =
  let p = local_pair () in
  ignore (Sys.opaque_identity (fst p));
  match t with
  | Leaf -> Leaf
  | Node (x, a, b) ->
      let[@tail_mod_cons] (self @ global) t = map_global_closure t ~f in
      Node (f x, self a, (self [@tailcall]) b)

let rec build_tree acc n =
  if n = 0 then acc else build_tree (Node (n, Leaf, acc)) (n - 1)

let map_global_closure t ~f =
  let rec to_list acc = function
    | Leaf -> List.rev acc
    | Node (x, _, b) -> to_list (x :: acc) b
  in
  to_list [] (map_global_closure (build_tree Leaf (List.length t)) ~f)

(* TMC, region + a parameter at local, and the recursive call is passed a
   value allocated in the region. The region must not be closed. *)
let[@tail_mod_cons] rec map_pass_local (local_ prev : int * int) t ~f =
  use_local prev;
  match t with
  | [] -> []
  | hd :: tl -> f (hd + fst prev) :: map_pass_local (stack_ (hd, hd)) tl ~f

(* TMC, region + a parameter at local, and the recursive call is passed a
   field of a parameter, but reading it boxes the float in the region. The
   region must not be closed. *)
type float_record = { v : float }

let[@tail_mod_cons] rec map_float_field (local_ r) (local_ x : float) t ~f =
  use_local (int_of_float x, 0);
  match t with
  | [] -> []
  | hd :: tl -> f hd :: map_float_field r r.v tl ~f

(* TMC, local closures (they capture a local value) calling themselves or each
   other. The closures are allocated outside the region of the function being
   called, so the region can be closed. Stack space constant. *)
let map_local_closure t ~f =
  let local_ offset = (Sys.opaque_identity 0, 0) in
  let[@tail_mod_cons] rec go t =
    let p = local_pair () in
    ignore (Sys.opaque_identity (fst p));
    match t with
    | [] -> []
    | hd :: tl -> f (hd + fst offset) :: go tl
  in
  go t [@nontail]

let map_local_closure_mutual t ~f =
  let local_ offset = (Sys.opaque_identity 0, 0) in
  let[@tail_mod_cons] rec go t =
    let p = local_pair () in
    ignore (Sys.opaque_identity (fst p));
    match t with
    | [] -> []
    | hd :: tl -> f (hd + fst offset) :: go' tl
  and[@tail_mod_cons] go' t =
    let p = local_pair () in
    ignore (Sys.opaque_identity (fst p));
    match t with
    | [] -> []
    | hd :: tl -> f (hd + snd offset) :: go tl
  in
  go t [@nontail]

(* TMC, a local closure passing a local value it captures at a local
   parameter. The captured value is allocated before the closure, so outside
   its region, and the region can be closed. Stack space constant. *)
let map_free_local t ~f =
  let local_ k = (Sys.opaque_identity 0, 0) in
  let[@tail_mod_cons] rec go (local_ ctx : int * int) t =
    let p = local_pair () in
    ignore (Sys.opaque_identity (fst p));
    use_local ctx;
    match t with
    | [] -> []
    | hd :: tl -> f (hd + fst ctx) :: go k tl
  in
  go k t [@nontail]

(* TMC, the callee [g] is a local closure allocated in the caller's region.
   The region must not be closed when calling [g], or [g] (and the [p] it
   captures) would be freed and overwritten by [_overwrite]. *)
let[@tail_mod_cons] rec call_local_closure (n : int) =
  let local_ p = (n, n) in
  let[@tail_mod_cons] rec g n =
    let local_ _overwrite = Sys.opaque_identity (100, n) in
    if n = 0 then [] else fst p :: g (n - 1)
  in
  if n = 0 then 0 :: g 1 else 0 :: call_local_closure (n - 1)

(* The TMC call in the body of a [try] must not close the region: this used to
   crash the compiler. *)
let[@tail_mod_cons] rec try_body n =
  let local_ p = (n, n) in
  try n :: try_body (n - 1) with _ -> [ fst p ]

(* TMC call under [Sys.opaque_identity]: the argument of [opaque] is in TMC
   position but not in tail position, so the call must not close the region.
   It is not a tailcall, so there is no warning. *)
let[@tail_mod_cons] rec map_opaque t ~f =
  let local_ p = (Sys.opaque_identity 0, 0) in
  use_local p;
  match t with
  | [] -> []
  | hd :: tl -> f hd :: Sys.opaque_identity (map_opaque tl ~f)

(* A method call in tail position is a tail call in the direct version, but
   the dps version writes its result to the destination, so the call must not
   close the region. *)
let[@tail_mod_cons] rec send_in_tail (o : < m : int list >) n =
  let local_ p = (Sys.opaque_identity n, n) in
  use_local p;
  if n = 0 then o#m else n :: send_in_tail o (n - 1)

(* As above, for a call on the right of [&&] or [||]. *)
type _ ty = Bool_and : bool ty | Bool_or : bool ty | List : int list ty

let[@inline never] big n = n > 3

let[@tail_mod_cons] rec seq_in_tail : type a. a ty -> int -> a = fun ty n ->
  let local_ p = (Sys.opaque_identity n, n) in
  use_local p;
  match ty with
  | Bool_and -> n > 0 && big n
  | Bool_or -> n < 0 || big n
  | List -> if n = 0 then [] else n :: seq_in_tail List (n - 1)

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

let separator = String.make 80 '='

let () =
  print_endline separator;
  print_endline "Functions that should be constant stack space:";
  print_endline separator;
  report "rev_map" (rev_map []);
  report "rev_map_with_local" (rev_map_with_local []);
  report "map_plain" map_plain;
  report "map_exclave" map_exclave;
  report "map_local" map_local;
  report "map_local_param" (map_local_param (1, 2));
  report "map_local_closure" map_local_closure;
  report "map_local_closure_mutual" map_local_closure_mutual;
  report "map_free_local" map_free_local;
  report "map_nested_regions" map_nested_regions;
  report "map_nested_regions_shared" map_nested_regions_shared;
  report "map_via_helper" map_via_helper;
  report "map_local_args" (fun t ~f ->
    map_local_args (1, 2) ~count:0 ~seen:[] t ~f:(fun x -> f x));
  report "map_tail_call" (map_tail_call (0, 0));
  report "map_global_args" (map_global_args (0, 0) Int.compare 0);
  report "map_global_closure" map_global_closure;

  print_endline separator;
  print_endline "Functions that should *not* be constant stack space";
  print_endline separator;
  report "map_using_local" map_using_local;
  report "map_pass_local" (map_pass_local (0, 0));
  report "map_float_field" (map_float_field { v = 1. } 1.);
  report "map_opaque" map_opaque;
  print_endline separator;


  let results = map_pass_local (0, 0) [1; 2; 3; 4] ~f:Fun.id in
  Printf.printf "map_pass_local results: %s\n"
    (String.concat " " (List.map string_of_int results));
  let results = map_opaque [1; 2; 3; 4] ~f:Fun.id in
  Printf.printf "map_opaque results: %s\n"
    (String.concat " " (List.map string_of_int results));
  let results = send_in_tail (object method m = [0] end) 3 in
  Printf.printf "send_in_tail results: %s\n"
    (String.concat " " (List.map string_of_int results));
  let results = seq_in_tail List 3 in
  Printf.printf "seq_in_tail results: %s %b %b\n"
    (String.concat " " (List.map string_of_int results))
    (seq_in_tail Bool_and 5) (seq_in_tail Bool_or 2);
  let results = call_local_closure 1 in
  Printf.printf "call_local_closure results: %s\n"
    (String.concat " " (List.map string_of_int results))
