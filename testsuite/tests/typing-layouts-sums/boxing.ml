(* TEST
 flags = "-extension layouts_alpha";
 modules = "boxed_boundary.mli boxed_boundary.ml";
 { bytecode; } { native; }
*)

external box : ('a : any). 'a -> 'a box = "%box" [@@layout_poly]
external unbox : ('a : any). 'a box -> 'a = "%unbox" [@@layout_poly]

type erased : void
external erase : unit -> erased = "%unbox_unit"
type ordinary = Empty | Mixed of int32_u * string ref * erased
              | Absent | Narrow of string ref

let[@inline never] pack (value : Boxed_boundary.t) = box value
let[@inline never] unpack value : Boxed_boundary.t = unbox value

let check_shape boxed ordinary =
  let actual = Obj.repr boxed and expected = Obj.repr ordinary in
  if Obj.is_int expected then assert (actual == expected)
  else begin
    assert (Obj.is_block actual);
    assert (Obj.tag actual = Obj.tag expected);
    assert (Obj.size actual = Obj.size expected);
    assert (Obj.Uniform_or_mixed.(repr (of_block actual) =
                                  repr (of_block expected)))
  end

let () =
  let open Boxed_boundary in
  let text = ref (String.make 3 'x') in
  let packed = [pack (empty ()); pack (mixed (-#77l) text);
                pack (absent ()); pack (narrow text)] in
  let ordinary = [Empty; Mixed (-#77l, text, erase ()); Absent; Narrow text] in
  Gc.compact ();
  List.iter2 check_shape packed ordinary;
  text := "after collection";
  let inspect_box boxed = inspect (unpack boxed) in
  let expected = [0l, None; -77l, Some !text; 1l, None; 2l, Some !text] in
  assert (List.map inspect_box packed = expected);
  assert (List.map (fun value -> inspect_box (Obj.magic value)) ordinary =
          expected);
  match List.map Obj.magic packed with
  | [Empty; Mixed (_, ptr, _); Absent; Narrow ptr'] ->
    assert (ptr == text && ptr' == text)
  | _ -> assert false

external monomorphic_box : Boxed_boundary.t -> Boxed_boundary.t box = "%box"
external monomorphic_unbox : Boxed_boundary.t box -> Boxed_boundary.t
  = "%unbox"

let[@inline never] only_strong_reference () =
  let pointer = ref (String.make 39 'x') in
  let weak = Weak.create 1 in
  Weak.set weak 0 (Some pointer);
  let pack = Sys.opaque_identity monomorphic_box in
  weak, pack (Boxed_boundary.mixed #19l pointer)

let () =
  let weak, packed = only_strong_reference () in
  Gc.compact ();
  assert (Weak.check weak 0);
  check_shape packed (Mixed (#19l, ref "", erase ()));
  let value = (Sys.opaque_identity monomorphic_unbox) packed in
  assert (Boxed_boundary.inspect value = (19l, Some (String.make 39 'x')))

external local_box : ('a : any). ('a[@local_opt]) -> ('a box[@local_opt])
  = "%box" [@@layout_poly]
external globalize : local_ 'a -> 'a = "%obj_dup"

let () =
  let text = ref "local" in
  let packed = stack_ (local_box (Boxed_boundary.narrow text)) in
  check_shape (globalize packed) (Narrow text)
