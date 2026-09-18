(* TEST
 modules = "local_bigstring_stubs.c";
 {
   bytecode;
 }{
   native;
 }
*)

open Bigarray

type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

external is_stack : ('a, 'b, 'c) Array1.t @ local -> bool
  = "caml_ba_is_stack" [@@noalloc]
external has_finalizer : ('a, 'b, 'c) Array1.t @ local -> bool
  = "local_bigstring_has_finalizer" [@@noalloc]
external owns_data : bigstring @ local -> bool
  = "local_bigstring_owns_data" [@@noalloc]

(* The public [Marshal] signature only accepts global values. *)
external marshal : 'a @ local -> Marshal.extern_flags list -> string
  = "caml_output_value_to_string"

let round_trip (v : 'a @ local) : 'a = Marshal.from_string (marshal v []) 0

let check_owned a =
  assert (not (is_stack a) && owns_data a && has_finalizer a)

let raises_invalid (f : (unit -> unit) @ local once) =
  match f () with
  | () -> false
  | exception Invalid_argument _ -> true

let collect () = Gc.full_major (); Gc.compact ()
let of_string s = Array1.init char c_layout (String.length s) (String.get s)
let to_string a = String.init (Array1.dim a) (fun i -> a.{i})

let mutate_view s ofs len c =
  let a = of_string s in
  (Array1.with_sub_local [@inlined never]) a ofs len (fun view ->
    Array1.fill view c);
  (* The backing storage remains usable after the local region has ended. *)
  collect ();
  to_string a

let test_views () =
  let mutate ofs len = mutate_view "----------" ofs len '#' in
  assert (mutate 4 5  = "----#####-");
  assert (mutate 0 3  = "###-------");
  assert (mutate 7 3  = "-------###");
  assert (mutate 0 10 = "##########");
  assert (mutate 0 0  = "----------");
  assert (mutate 10 0 = "----------")

let test_bounds () =
  let a = of_string "XXXXXXX" in
  for first = -1 to 8 do
    for last = -1 to 8 do
      let len = last - first in
      let heap = raises_invalid (fun () -> ignore (Array1.sub a first len)) in
      let local = raises_invalid (fun () ->
        Array1.with_sub_local a first len (fun _ -> ())) in
      assert (heap = local)
    done
  done

let marshal_shared_views s ofs len =
  let a = of_string s in
  let x, y = Array1.with_sub_local a ofs len (fun view ->
    Array1.with_sub_local view 0 len (fun alias ->
      assert (view != alias);
      Array1.fill view '#';
      round_trip (stack_ (view, alias)) [@nontail]) [@nontail]) in
  collect ();
  (* A stack-allocated bigstring demarshals into a heap-allocated one that owns
     its memory *)
  check_owned x;
  check_owned y;
  assert (to_string x = String.make len '#');
  assert (to_string y = String.make len '#');
  (* Each demarshalled bigstring has its own copy of the memory *)
  Array1.fill x 'A';
  Array1.fill y 'B';
  to_string a, to_string x, to_string y

let test_marshal () =
  assert (marshal_shared_views "----------" 4 5
  = ("----#####-", "AAAAA", "BBBBB"));
  assert (marshal_shared_views "----------" 0 3
  = ("###-------", "AAA"  , "BBB"))

let[@inline never] make_backing weak =
  let a = of_string "abcdefg" in
  Weak.set weak 0 (Some a);
  a

let check_live weak (view : bigstring @ local) =
  collect ();
  assert (Weak.check weak 0 && view.{0} = 'b')

(* The bigstring argument to [with_sub_local] is live within the callback's
   scope, but can be collected after the function exits. *)

let[@inline never] normal_scope weak =
  Array1.with_sub_local (make_backing weak) 1 3 (fun view ->
    collect ();
    check_live weak view;
    String.make 3 'R')

let[@inline never] exceptional_scope weak =
  Array1.with_sub_local (make_backing weak) 1 3 (fun view ->
    collect ();
    check_live weak view;
    raise Exit)

let test_lifetime () =
  let weak = Weak.create 1 in
  assert (normal_scope weak = "RRR");
  collect ();
  assert (not (Weak.check weak 0));
  begin match exceptional_scope weak with
  | _ -> failwith "callback exception was lost"
  | exception Exit -> ()
  end;
  collect ();
  assert (not (Weak.check weak 0))

let () =
  test_views ();
  test_bounds ();
  test_marshal ();
  test_lifetime ()
