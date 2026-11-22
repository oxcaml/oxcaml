(* TEST
 flags = "-extension layouts_alpha";
 modules = "tag_boxes.ml";
 { bytecode; } { native; }
*)

type inner = Off | On of string

module Flat = struct
  type t = Empty | Entry of inner#
  include Tag_boxes.Flat (struct type nonrec t = t# end)
  let read = function Empty -> None | Entry Off -> Some "off"
    | Entry (On s) -> Some s
  let read_inline = function #Empty -> None | #Entry Off -> Some "off"
    | #Entry (On s) -> Some s
  let check ordinary inline expected =
    assert (read ordinary = expected);
    assert (read (pack inline) = expected);
    assert (read_inline (unpack ordinary) = expected)
  let () = check Empty #Empty None; check (Entry Off) (#Entry Off) (Some "off");
    check (Entry (On "on")) (#Entry (On "on")) (Some "on")
end

module Named = struct
  type field = #{ inner : inner# }
  type t = Empty | Entry of field
  include Tag_boxes.Flat (struct type nonrec t = t# end)
  let read = function Empty -> "empty" | Entry #{ inner = Off } -> "off"
    | Entry #{ inner = On s } -> s
  let read_inline = function #Empty -> "empty" | #Entry #{ inner = Off } -> "off"
    | #Entry #{ inner = On s } -> s
  let check ordinary inline expected =
    assert (read (pack inline) = expected);
    assert (read_inline (unpack ordinary) = expected)
  let () = check Empty #Empty "empty";
    check (Entry #{ inner = Off }) (#Entry #{ inner = Off }) "off";
    check (Entry #{ inner = On "on" }) (#Entry #{ inner = On "on" }) "on"
end

module Grouped = struct
  type t = Empty | Entry of { mutable inner : inner# }
  include Tag_boxes.Grouped (struct type nonrec t = t# end)
  let check ordinary inline expected =
    let read = function #Empty -> None | #Entry { inner = Off } -> Some "off"
      | #Entry { inner = On s } -> Some s in
    assert (read (unpack ordinary) = expected);
    List.iter (function
      | Empty -> assert (expected = None)
      | Entry r as box ->
        assert (Obj.size (Obj.repr box) = 2);
        assert (match r.inner with Off -> expected = Some "off"
                                  | On s -> expected = Some s);
        let alias = Sys.opaque_identity (Entry r) in
        (match alias with Entry a -> a.inner <- On "changed" | Empty -> ());
        Gc.compact ();
        assert (read (unpack box) = Some "changed")) [ordinary; pack inline]
  let () = check Empty #Empty None;
    check (Entry { inner = Off }) (#Entry { inner = Off }) (Some "off");
    check (Entry { inner = On "on" }) (#Entry { inner = On "on" }) (Some "on")
end

module Product = struct
  type t = Empty | Entry of { marker : unit#; mutable inner : inner# }
  include Tag_boxes.Product (struct type nonrec t = t# end)
  let check ordinary inline expected =
    let read = function #Empty -> None | #Entry { inner = Off; _ } -> Some "off"
      | #Entry { inner = On s; _ } -> Some s in
    assert (read (unpack ordinary) = expected);
    List.iter (function
      | Empty -> assert (expected = None)
      | Entry r as box ->
        let block = Obj.repr (Sys.opaque_identity box) in
        assert (Obj.tag block = 0 && Obj.size block = 2);
        (match Sys.backend_type with
         | Bytecode -> assert (Obj.is_block (Obj.field block 1));
           assert (Obj.size (Obj.field block 1) = 2)
         | _ -> ());
        assert (match r.inner with Off -> expected = Some "off"
                                  | On s -> expected = Some s);
        let alias = Sys.opaque_identity (Entry r) in
        (match alias with Entry a -> a.inner <- On "changed" | Empty -> ());
        Gc.compact ();
        assert (read (unpack box) = Some "changed")) [ordinary; pack inline]
  let () = check Empty #Empty None;
    check (Entry { marker = #(); inner = Off })
      (#Entry { marker = #(); inner = Off }) (Some "off");
    check (Entry { marker = #(); inner = On "on" })
      (#Entry { marker = #(); inner = On "on" }) (Some "on")
end

module Deep = struct
  type t = Stop | Next of Grouped.t#
  include Tag_boxes.Deep (struct type nonrec t = t# end)
  let read = function Stop -> "stop" | Next Empty -> "empty"
    | Next (Entry { inner = Off }) -> "off"
    | Next (Entry { inner = On s }) -> s
  let read_inline = function #Stop -> "stop" | #Next Empty -> "empty"
    | #Next (Entry { inner = Off }) -> "off"
    | #Next (Entry { inner = On s }) -> s
  let check ordinary inline expected =
    assert (read (pack inline) = expected);
    assert (read_inline (unpack ordinary) = expected)
  let () = check Stop #Stop "stop"; check (Next Empty) (#Next Empty) "empty";
    check (Next (Entry { inner = Off })) (#Next (Entry { inner = Off })) "off";
    check (Next (Entry { inner = On "on" }))
      (#Next (Entry { inner = On "on" })) "on"
end

type mixed = Unwrap of inner# | Record of { label : string; number : int }
  [@@unboxed]

let read_mixed = function
  | Unwrap Off -> "off"
  | Unwrap (On s) -> s
  | Record r -> r.#label ^ string_of_int r.#number

let () =
  assert (read_mixed (Unwrap Off) = "off");
  assert (read_mixed (Unwrap (On "on")) = "on");
  assert (read_mixed (Record { label = "n="; number = 4 }) = "n=4")
