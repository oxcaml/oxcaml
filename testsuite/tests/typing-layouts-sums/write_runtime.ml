(* TEST
 include stdlib_stable;
 {
   bytecode;
 } {
   native;
 }
*)

open Stdlib_stable

type choice = Empty | Text of string | Number of int64_u [@@unboxed]
type cell = { mutable data : choice; sibling : string }

external box_int64 : int64_u -> int64 = "%box_int64"

let[@inline never] by_index cell value = Idx_mut.set cell (.data) value
let[@inline never] by_address (addr : choice addr) value = Addr.set addr value

let[@inline never] check_text cell =
  match cell.data with
  | Text s -> assert (s = "next")
  | Empty | Number _ -> assert false

let[@inline never] check_number cell =
  match cell.data with
  | Number n -> assert (box_int64 n = 31L)
  | Empty | Text _ -> assert false

let () =
  let cell = { data = Empty; sibling = "sibling" } in
  let addr = Addr.of_idx cell (.data) in
  Gc.compact ();
  by_index cell (Text (String.concat "" ["ne"; "xt"]));
  Gc.minor ();
  check_text cell;
  by_address addr (Number #31L);
  check_number cell;
  by_address addr (Text (String.concat "" ["n"; "ext"]));
  Gc.minor ();
  check_text cell;
  by_index cell (Number #31L);
  check_number cell;
  assert (cell.sibling = "sibling")
