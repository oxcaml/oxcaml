(* TEST
 modules = "stubs.c";
 reference = "${test_source_directory}/elide_void_runtime.reference";
 native;
 flambda2;
 {
   native;
 }{
   flags = "-Oclassic";
   native;
 }{
   flags = "-O3";
   native;
 }{
   bytecode;
 }
*)

(* Arguments marked [@elide_void] are not passed to externals. The stubs
   in [stubs.c] concatenate their int arguments as decimal digits, so a void
   argument passed in bytecode (as unit, i.e. 0) changes the result. *)

type w : void
external w : unit -> w = "%unbox_unit"

let log = ref []
let note x = log := x :: !log
let[@inline never] noisy_w name = note name; w ()
let raise_exit () = if Sys.opaque_identity true then raise Exit
let value name x = note name; x
let check_order expected f =
  log := [];
  f ();
  assert (List.rev !log = expected)

(* C stubs *)

external first : (w [@elide_void]) -> int -> int -> int = "elide_digits2"
external mid : int -> (w [@elide_void]) -> int -> int = "elide_digits2"
external last : int -> int -> (w [@elide_void]) -> int = "elide_digits2"
external lab : a:int -> w:(w [@elide_void]) -> b:int -> int
  = "elide_digits2"
(* The unannotated void argument is still passed in bytecode. *)
external mixed : (w [@elide_void]) -> w -> int -> int
  = "elide_digits2" "elide_digits1"
(* The bytecode stub receives [argn], which the result encodes. *)
external seven_last :
  int -> int -> int -> int -> int -> int -> (w [@elide_void]) -> int
  = "elide_digits_argv" "elide_digits6"
(* Six source arguments but three runtime ones, so one name suffices. *)
external six_source :
  int -> (w [@elide_void]) -> int -> (w [@elide_void]) -> int
  -> (w [@elide_void]) -> int
  = "elide_digits3"
(* Global [@@unboxed] does not apply to the elided argument. *)
external hypot : float -> (w [@elide_void]) -> float -> float
  = "caml_hypot_float" "caml_hypot" [@@unboxed] [@@noalloc]

let[@inline never] apply_mid (f : int -> w -> int -> int) = f 1 (w ()) 2
let[@inline never] ordinary ~a ~w:(_ : w) ~b = a * 10 + b

let () =
  assert (first (w ()) 1 2 = 12);
  assert (mid 1 (w ()) 2 = 12);
  assert (lab ~b:2 ~w:(w ()) ~a:1 = 12);
  assert (mixed (w ()) (w ()) 6 = 6);
  assert (seven_last 1 2 3 4 5 6 (w ()) = 60123456);
  assert (six_source 1 (w ()) 2 (w ()) 3 (w ()) = 123);
  assert (hypot 3. (w ()) 4. = 5.);
  print_endline "C stubs: ok"

let () =
  let before_void = Sys.opaque_identity (mid 1) in
  let after_void = Sys.opaque_identity (mid 1 (w ())) in
  let lab' = Sys.opaque_identity (lab ~w:(w ())) in
  let mixed' = Sys.opaque_identity mixed in
  let seven_last' = Sys.opaque_identity seven_last in
  let six_source' = Sys.opaque_identity six_source in
  let hypot' = Sys.opaque_identity hypot in
  assert (before_void (w ()) 2 = 12);
  assert (after_void 2 = 12);
  assert (lab' ~a:1 ~b:2 = 12);
  assert (apply_mid mid = 12);
  assert (mixed' (w ()) (w ()) 6 = 6);
  assert (seven_last' 1 2 3 4 5 6 (w ()) = 60123456);
  assert (six_source' 1 (w ()) 2 (w ()) 3 (w ()) = 123);
  assert (hypot' 3. (w ()) 4. = 5.);
  print_endline "eta and partial: ok"

let () =
  (* Arguments are evaluated right-to-left, with witnesses interleaved. *)
  check_order ["b"; "w"; "a"] (fun () ->
    assert (ordinary ~b:(value "b" 2) ~w:(noisy_w "w") ~a:(value "a" 1) = 12));
  check_order ["b"; "a"; "w"] (fun () ->
    assert (first (noisy_w "w") (value "a" 1) (value "b" 2) = 12));
  check_order ["b"; "w"; "a"] (fun () ->
    assert (mid (value "a" 1) (noisy_w "w") (value "b" 2) = 12));
  check_order ["w"; "b"; "a"] (fun () ->
    assert (last (value "a" 1) (value "b" 2) (noisy_w "w") = 12));
  check_order ["w3"; "c"; "w2"; "b"; "w1"; "a"] (fun () ->
    assert (six_source (value "a" 1) (noisy_w "w1") (value "b" 2)
              (noisy_w "w2") (value "c" 3) (noisy_w "w3") = 123));
  check_order ["b"; "w"; "a"] (fun () ->
    assert (lab ~b:(value "b" 2) ~w:(noisy_w "w") ~a:(value "a" 1) = 12));
  check_order ["b"; "w"; "a"] (fun () ->
    assert (hypot (value "a" 3.) (noisy_w "w") (value "b" 4.) = 5.));
  (* Direct calls agree with eta-expanded calls and staged applications. *)
  let mid' = Sys.opaque_identity mid in
  check_order ["b"; "w"; "a"] (fun () ->
    assert (mid' (value "a" 1) (noisy_w "w") (value "b" 2) = 12));
  check_order ["w"; "a"; "b"] (fun () ->
    let f = mid (value "a" 1) (noisy_w "w") in
    assert (f (value "b" 2) = 12));
  (* A retained read must happen before a witness mutates its source. *)
  let mutable b = 2 in
  assert (mid 1 (b <- 3; w ()) b = 12);
  assert (b = 3);
  (* Exceptions stop evaluation at the same point. *)
  check_order ["b"; "w"] (fun () ->
    match mid (value "a" 1) (note "w"; raise_exit (); w ()) (value "b" 2) with
    | _ -> assert false
    | exception Exit -> ());
  print_endline "effects: ok"

(* Primitives *)

external id_last : string -> (w [@elide_void]) -> string = "%identity"
external add_mid : int -> (w [@elide_void]) -> int -> int = "%addint"
external opaque : (w [@elide_void]) -> 'a -> 'a = "%opaque"
external equal : 'a -> (w [@elide_void]) -> 'a -> bool = "%equal"
external file : (w [@elide_void]) -> string = "%loc_FILE"
external argv : (w [@elide_void]) -> string array = "%sys_argv"
external ( &&& ) : bool -> (w [@elide_void]) -> bool -> bool = "%sequand"
external ( ||| ) : (w [@elide_void]) -> bool -> bool -> bool = "%sequor"

let () =
  let s = "hello" in
  let t = Sys.opaque_identity true and f = Sys.opaque_identity false in
  assert (id_last s (w ()) == s);
  assert (add_mid 1 (w ()) 2 = 3);
  assert (opaque (w ()) s == s);
  assert (equal 1 (w ()) 1 && not (equal "a" (w ()) "b"));
  assert (file (w ()) = __FILE__);
  assert (argv (w ()) = Sys.argv);
  assert (( &&& ) t (w ()) t && not (( &&& ) t (w ()) f));
  assert (( ||| ) (w ()) f t && not (( ||| ) (w ()) f f));
  (* The last argument of a full application is evaluated lazily. *)
  log := [];
  assert (not (( &&& ) f (w ()) (note "rhs"; t)));
  assert (( ||| ) (w ()) t (note "rhs"; f));
  assert (!log = []);
  let add_one = Sys.opaque_identity (add_mid 1 (w ())) in
  let equal' = Sys.opaque_identity equal in
  let and' = Sys.opaque_identity ( &&& ) in
  assert (add_one 2 = 3);
  assert (equal' 1 (w ()) 1);
  assert (not (and' f (w ()) t));
  assert (id_last s (noisy_w "w") == s && opaque (noisy_w "w") s == s);
  assert (!log = ["w"; "w"]);
  check_order ["b"; "w"; "a"] (fun () ->
    assert (add_mid (value "a" 1) (noisy_w "w") (value "b" 2) = 3));
  check_order ["value"; "w"] (fun () ->
    assert (opaque (noisy_w "w") (value "value" s) == s));
  (* For [%sequand] and [%sequor]:
     - evaluate first all witnesses
     - then the LHS
     - then lazily the RHS. *)
  check_order ["w"; "lhs"] (fun () ->
    assert (( ||| ) (noisy_w "w") (value "lhs" t) (value "rhs" f)));
  check_order ["w"; "lhs"; "rhs"] (fun () ->
    assert (( ||| ) (noisy_w "w") (value "lhs" f) (value "rhs" t)));
  check_order ["w"; "lhs"; "rhs"] (fun () ->
    assert (( &&& ) (value "lhs" t) (noisy_w "w") (value "rhs" t)));
  (match add_mid 1 (raise_exit (); w ()) 2 with
   | _ -> assert false
   | exception Exit -> ());
  print_endline "primitives: ok"

(* Specialization *)

(* Unboxed arrays need specialization from the retained arguments' types, so
   witnesses at any position must be skipped, including past four source
   arrows and after the last retained argument. *)
external box : float# -> float = "%box_float"
external length :
  ('a : any mod separable). (w [@elide_void]) -> 'a array -> int
  = "%array_length" [@@layout_poly]
external get :
  ('a : any mod separable).
    (w [@elide_void]) -> 'a array -> (w [@elide_void]) -> int
    -> (w [@elide_void]) -> 'a
  = "%array_safe_get" [@@layout_poly]
external set :
  ('a : any mod separable).
    'a array -> (w [@elide_void]) -> int -> 'a -> (w [@elide_void])
    -> unit
  = "%array_safe_set" [@@layout_poly]
external make_uninit :
  ('a : any mod separable). int -> (w [@elide_void]) -> 'a array
  = "%makearray_dynamic_uninit" [@@layout_poly]
(* An optional argument before a witness. *)
external same : ?o:int -> (w [@elide_void]) -> int option -> bool
  = "%equal"

(* The coercion is eta-expanded at the signature's type. *)
module M : sig
  type t
  val of_array : float# array -> t
  val get : w -> t -> w -> int -> w -> float#
end = struct
  type t = float# array
  let of_array a = a
  external get :
    (w [@elide_void]) -> t -> (w [@elide_void]) -> int
    -> (w [@elide_void]) -> float#
    = "%array_safe_get"
end

let () =
  let a : float# array = make_uninit 2 (w ()) in
  set a (w ()) 0 #1. (w ());
  set a (w ()) 1 #2. (w ());
  assert (length (w ()) a = 2);
  assert (box (get (w ()) a (w ()) 1 (w ())) = 2.);
  let get_a = Sys.opaque_identity (get (w ()) a) in
  assert (box (get_a (w ()) 0 (w ())) = 1.);
  assert (box (M.get (w ()) (M.of_array a) (w ()) 1 (w ())) = 2.);
  (* Do not peel the arrow in a function-valued array element. *)
  assert (get (w ()) [| succ |] (w ()) 0 (w ()) 41 = 42);
  assert (same (w ()) None && same ~o:1 (w ()) (Some 1));
  assert (not (same ~o:1 (w ()) None));
  print_endline "specialization: ok"
