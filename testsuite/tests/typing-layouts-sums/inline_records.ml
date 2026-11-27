(* TEST
 flags = "-extension layouts_alpha";
 modules = "named_arms.mli named_arms.ml";
 { bytecode; } { native; }
*)

type direct = End | Entry of { pair : int * int; key : int } [@@unboxed]

let check_direct = function
  | Entry { pair = (2, value); key = 9 } -> value
  | Entry record -> record.#key
  | End -> 0

let () =
  assert (check_direct (Entry { pair = 2, 3; key = 9 }) = 3);
  assert (check_direct (Entry { pair = 0, 1; key = 5 }) = 5)

let make_direct_box key : direct box = Entry { pair = 1, 2; key }
let () = match make_direct_box 11 with
  | Entry { key = 11; _ } -> ()
  | Entry _ | End -> assert false

open Named_arms

external box32 : int32_u -> int32 = "%box_int32"
external box64 : int64_u -> int64 = "%box_int64"
external pack_hidden : hidden -> hidden box = "%box"

let inspect = function
  | #Stop -> assert false
  | #Data { wide; narrow; count; text } -> box64 wide, box32 narrow, count, text

let () =
  let ordinary = Data { text = "before"; count = 13; narrow = -#17l;
                        wide = #4886718345L } in
  let alias = match ordinary with Data record -> Data record | Stop -> Stop in
  (match alias with Data record -> record.text <- "after" | Stop -> ());
  let expected = 4886718345L, -17l, 13, "after" in
  assert (inspect (copy (reveal (hide (unpack ordinary)))) = expected);
  let from_hidden = boxed_view (pack_hidden (hide (unpack ordinary))) in
  assert (inspect (unpack from_hidden) = expected);
  let boxed = pack (#Data { wide = #4886718345L; count = 13;
                            text = "after"; narrow = -#17l }) in
  Gc.compact ();
  match boxed with
  | Data { text; count; narrow; wide } ->
    assert ((box64 wide, box32 narrow, count, text) = expected)
  | Stop -> assert false

type inner = Off | On of string
type nested = Empty | Single of { inner : inner# } [@@unboxed]

let read_nested = function
  | Empty -> "empty"
  | Single { inner = Off } -> "off"
  | Single { inner = On text } -> text

let () = assert (read_nested (Single { inner = On "on" }) = "on")

type ordinary_nested = Empty | Single of { inner : inner# }
external pack_nested : nested -> nested box = "%box"

let check_block value words =
  let block = Obj.repr (Sys.opaque_identity value) in
  assert (Obj.is_block block && Obj.tag block = 0 && Obj.size block = words)

let () =
  check_block (Single { inner = Off } : ordinary_nested) 2;
  check_block (Single { inner = On "on" } : ordinary_nested) 2;
  check_block (pack_nested (Single { inner = Off })) 2;
  check_block (pack_nested (Single { inner = On "on" })) 2

type nested_product = Absent | Present of { inner : inner#; count : int }
external box_product : nested_product# -> nested_product = "%box"

let () = match box_product (#Present { inner = On "on"; count = 9 }) with
  | Present { inner = On "on"; count = 9 } -> ()
  | Absent | Present _ -> assert false

let mutate_box (value : t# box) =
  match value with Stop -> () | Data record -> record.text <- "changed"

let () =
  let boxed = Data { narrow = #1l; text = "old"; wide = #2L; count = 3 } in
  mutate_box boxed;
  assert (inspect (unpack boxed) = (2L, 1l, 3, "changed"))

type counter = Zero | Counter of { mutable n : int }
external box_counter : counter# -> counter = "%box"
external unbox_counter : counter -> counter# = "%unbox"

let make_counter () = box_counter (#Counter { n = 0 })
let count boxed = match unbox_counter boxed with #Zero -> -1 | #Counter r -> r.#n

let () =
  let first = make_counter () and second = make_counter () in
  check_block first 1;
  assert (count first = 0);
  (match first with Counter r -> r.n <- 42 | Zero -> ());
  assert (count first = 42);
  assert (count second = 0)
