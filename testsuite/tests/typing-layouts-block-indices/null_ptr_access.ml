(* TEST
 reference = "${test_source_directory}/null_ptr_access.reference";
 include stdlib_stable;
 include stdlib_upstream_compatible;
 modules = "ptr_of_value.c";
 flambda2;
 arch_amd64;
 native;
*)

module Int64_u = struct
  type t = int64#

  external to_int64 : t -> (int64[@local_opt]) @@ portable =
    "%box_int64" [@@warning "-187"]

  external of_int64 : (int64[@local_opt]) -> t @@ portable =
    "%unbox_int64" [@@warning "-187"]

  let[@inline always] neg x = of_int64 (Int64.neg (to_int64 x))

  let[@inline always] add x y = of_int64 (Int64.add (to_int64 x) (to_int64 y))
end

type nothing = |

external get_ptr_local
  : ('a : any).
  #(nothing or_null * int64#) @ local -> 'a @ local
  = "%unsafe_get_ptr"
[@@layout_poly]

external set_ptr_local
  : ('a : any).
  #(nothing or_null * int64#) @ local -> 'a @ local -> unit
  = "%unsafe_set_ptr"
[@@layout_poly]

external addr_of_value
  : ('a : value_or_null).
  'a @ local -> int64#
  = "" "caml_native_pointer_of_value"

type pt = { x : int; y : int }

let[@inline never] set_y pt y =
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  set_ptr_local #(Null, y_addr) y

let[@inline never] get_y pt =
  exclave_
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  get_ptr_local #(Null, y_addr)

let () =
  let pt = stack_ { x = 10; y = 20 } in
  let i : int = get_y pt in
  Printf.printf "expected 20, got %d\n" i;
  set_y pt 200;
  Printf.printf "expected (10, 200), got (%d, %d)\n" pt.x pt.y;
  let (_ : pt) = Sys.opaque_identity pt in (* make sure the [pt] stays live *)
  ()
