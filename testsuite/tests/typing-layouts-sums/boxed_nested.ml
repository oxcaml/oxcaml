(* TEST
 flags = "-extension layouts_alpha";
 { bytecode; } { native; }
*)

external box : ('a : any). 'a -> 'a box = "%box" [@@layout_poly]
external unbox : ('a : any). 'a box -> 'a = "%unbox" [@@layout_poly]

type inner = Done | One of string | Two of string * string
type outer = Count of int | Inside of inner#
type flat = F_count of int | F_done | F_one of string | F_two of string * string

let[@inline never] pack (x : outer#) : outer = box x
let[@inline never] unpack (x : outer) : outer# = unbox x

let[@inline never] read (x : outer) =
  match x with
  | Count n -> string_of_int n
  | Inside Done -> "done"
  | Inside (One a) -> a
  | Inside (Two (a, b)) -> a ^ b

let check (ordinary : outer) (inline : outer#) flat text =
  let check_box value =
    let actual = Obj.repr value and expected = Obj.repr flat in
    if Obj.is_int expected then assert (actual == expected)
    else begin
      assert (Obj.is_block actual);
      assert (Obj.tag actual = Obj.tag expected);
      assert (Obj.size actual = Obj.size expected)
    end;
    assert (read value = text)
  in
  check_box ordinary;
  check_box (pack inline);
  check_box (pack (unpack ordinary))

let () =
  let a = String.make 3 'a' and b = String.make 2 'b' in
  check (Count 17) (Count 17 : outer#) (F_count 17) "17";
  check (Inside Done) (Inside Done : outer#) F_done "done";
  check (Inside (One a)) (Inside (One a) : outer#) (F_one a) a;
  check (Inside (Two (a, b))) (Inside (Two (a, b)) : outer#)
    (F_two (a, b)) (a ^ b)
