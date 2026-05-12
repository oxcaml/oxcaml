(* TEST
 flags = "-extension layouts_beta";
 include stdlib_upstream_compatible;
 native;
*)

(* Verify the runtime tag of [Record_float_block] is 253 ([Double_tag]). *)

type t = { f : float# }
let () =
  let r = { f = #3.14 } in
  assert (Obj.tag (Obj.repr r) = Obj.double_tag);
  assert (Obj.size (Obj.repr r) = 1);
  assert (Stdlib_upstream_compatible.Float_u.to_float r.f = 3.14)

(* For comparison: a [Record_ufloat] (tag-254 flat float-array). *)
type tu = { fu : float# } [@@represent_as_float_array]
let () =
  let r = { fu = #1.5 } in
  assert (Obj.tag (Obj.repr r) = Obj.double_array_tag)

(* Multi-field records remain mixed blocks (tag 0). *)
type tm = { mf : float#; mg : float# }
let () =
  let r = { mf = #1.0; mg = #2.0 } in
  assert (Obj.tag (Obj.repr r) = 0)

(* Mutable single-[float#] records also use [Record_float_block] (tag 253),
   and mutation works in place. *)
type tmu = { mutable mfu : float# }
let () =
  let r = { mfu = #1.0 } in
  assert (Obj.tag (Obj.repr r) = Obj.double_tag);
  assert (Obj.size (Obj.repr r) = 1);
  r.mfu <- #4.25;
  assert (Obj.tag (Obj.repr r) = Obj.double_tag);
  assert (Stdlib_upstream_compatible.Float_u.to_float r.mfu = 4.25)
