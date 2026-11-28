(* TEST
 {
   reference = "${test_source_directory}/curry.heap.reference";
   bytecode;
 }{
   stack-allocation;
   reference = "${test_source_directory}/curry.stack.reference";
   native;
 }{
   no-stack-allocation;
   reference = "${test_source_directory}/curry.heap.reference";
   native;
 }
*)

module M : sig
  (* explicit signature to force return modes *)
  val part_local : int -> int -> string @ local -> int -> int -> int -> int list
  val all_heap : int -> int -> string -> int -> int -> int -> int list
end = struct
  let part_local a b (c @ local) d e f =
    (* This function is used to test that its partial applications up
       to the second argument are heap allocated, then further partial
       applications to more arguments are stack allocated. If the 'c'
       argument is unused, the reaper can detect it and remove it from
       the closure of the partial application. All the other arguments
       are statically known, so the partial applications ends up being
       statically allocated. *)
    let _ = Sys.opaque_identity c in
    [a; b; d; e; f]
  let all_heap a b c d e f = [a;b;int_of_string c;d;e;f]
end


external is_stack : 'a @ local -> bool = "caml_obj_is_stack"
let loc (x @ local) =
  if is_stack x then 1 else 0

let[@inline never] flocal (arg @ local) =
  let g = M.part_local in
  let a = g 1 in
  let b = a 2 in
  let c = b arg in
  let d = c 4 in
  let e = d 5 in
  Gc.minor ();
  Printf.printf "%20s: %d%d%d%d%d%d\n"
    "specialise local"
    (loc g) (loc a) (loc b) (loc c) (loc d) (loc e)

let _ = flocal "asdf"

let fopaque () =
  let g = Sys.opaque_identity M.part_local in
  let a = g 1 in
  let b = a 2 in
  let c = b ("asdf" : @ local) in
  let d = c 4 in
  let e = d 5 in
  Gc.minor ();
  Printf.printf "%20s: %d%d%d%d%d%d\n"
    "caml_curry local"
    (loc g) (loc a) (loc b) (loc c) (loc d) (loc e)

let _ = fopaque ()


let[@inline never] fheap () =
  let g @ local = M.all_heap  in
  let a @ local = g 1  in
  let b @ local = a 2  in
  let c @ local = b "asdf"  in
  let d @ local = c 4  in
  let e @ local = d 5  in
  Gc.minor ();
  Printf.printf "%20s: %d%d%d%d%d%d\n"
    "all heap"
    (loc g) (loc a) (loc b) (loc c) (loc d) (loc e)

let () = fheap ()

module N : sig
  val local_curry : int -> (int -> int -> int) @ local
end = struct
  let local_curry a b c = a + b + c
end

let[@inline never] flocal_curry () =
  let n = Sys.opaque_identity 1 in
  let a = N.local_curry n in
  let b = a n in
  let g = Sys.opaque_identity N.local_curry in
  let c = g n in
  let d = c n in
  let e = g n n in
  Gc.minor ();
  Printf.printf "%20s: %d%d%d%d%d\n"
    "local curry"
    (loc a) (loc b) (loc c) (loc d) (loc e)

let () = flocal_curry ()

(* partial application of a local function *)
let[@inline never] f z =
  let (g @ local) a b c d e f = [a;b;c;d;e;f;z] in
  let a = g 1 in
  let b = a 2 in
  let c = b 3 in
  let d = c 4 in
  let e = d 5 in
  Gc.minor ();
  Printf.printf "%20s: %d%d%d%d%d%d\n"
    "all local"
    (loc g) (loc a) (loc b) (loc c) (loc d) (loc e)

let () = f 42

let[@inline never] tupled : (string*string) -> string @ local =
  fun (a,b) -> exclave_ (a^b)
let () =
  match tupled ("a","b") with
  | "ab" -> Printf.printf "%20s: ok\n" "tupled"
  | _ -> assert false

let globalref = ref 42
type 'a glob = { g : 'a @@ global }
let escape x = { g = x }.g
let[@inline never] prim () =
  let r = stack_ (ref 42) in
  (* currying of primitives (opaque goes via caml_curry) *)
  let a,b,c,d =
    ((Sys.opaque_identity (:=)) r,
     ((:=) r),
     escape ((Sys.opaque_identity (:=)) globalref),
     escape ((:=) globalref)) in
  Gc.minor ();
  Printf.printf "%20s: %d%d%d%d\n" "primcurry"
    (loc a) (loc b) (loc c) (loc d)

let () = prim ()

let curried (x @ local) = { g = fun () -> 42 }
let () =
  let _ = (Sys.opaque_identity curried) (stack_ (ref 42)) in
  Gc.minor ()
