(* TEST
 include ocamlcommon;
 bytecode;
*)

type witness : void

external set : (witness [@elide_void]) -> int array @ write corrupted ->
  int -> int -> unit = "%array_safe_set"

external copy : int array -> (witness [@elide_void]) -> int ->
  int array @ write corrupted -> int -> int -> unit = "%arrayblit"

type choice = First | Second of int32_u [@@unboxed]

external replace : (witness [@elide_void]) -> choice array @ write uncontended
  -> int -> choice -> unit = "%array_safe_set"

(* Specialization gives only the destination a fresh contention mode. *)
let rec changed_arguments index before after =
  match Types.get_desc before, Types.get_desc after with
  | Tarrow ((_, m, _), _, rest, _), Tarrow ((_, m', _), _, rest', _) ->
    let tail = changed_arguments (index + 1) rest rest' in
    if m == m' then tail else index :: tail
  | _ -> []

let primitive signature name =
  List.find_map (function
    | Types.Sig_value (id, { val_kind = Val_prim prim; val_type; _ }, _)
      when Ident.name id = name -> Some (prim, val_type)
    | _ -> None) signature |> Option.get

let check signature name expected =
  let prim, ty = primitive signature name in
  let specialized = Ctype.instance_primitive_destination Env.empty prim ty in
  match changed_arguments 0 ty specialized with
  | [index] when index = expected -> ()
  | indices -> failwith (Printf.sprintf "%s: expected argument %d, got [%s]"
      name expected (String.concat ", " (List.map string_of_int indices)))

let () =
  let cmi = Cmi_format.read_cmi "test_primitive_destination.cmi" in
  let signature, _ = cmi.cmi_sign in
  check signature "set" 1;
  check signature "copy" 3;
  let env = Env.add_signature signature (Lazy.force Env.initial) in
  let prim, ty = primitive signature "replace" in
  let checked = ref [] in
  let argument_loc index = checked := index :: !checked; Location.none in
  Ctype.check_primitive_sum_access (Direct_application argument_loc)
    ~loc:Location.none env prim ty;
  assert (!checked = [1])
