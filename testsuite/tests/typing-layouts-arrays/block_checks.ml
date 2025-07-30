(* Helper functions for checking block properties *)

(* Check if a block is a mixed block with given scannable prefix length *)
let check_mixed_block_scannable_size ~array_type obj expected_scannable_size =
  match Sys.backend_type with
  | Native ->
    let mixed_info = Obj.Uniform_or_mixed.of_block obj in
    begin match Obj.Uniform_or_mixed.repr mixed_info with
    | Uniform ->
      Printf.printf "%s: Expected mixed block, but got uniform block\n"
        array_type;
      assert false  (* Should be a mixed block *)
    | Mixed { scannable_prefix_len } ->
      assert (scannable_prefix_len = expected_scannable_size)
    end
  | Bytecode | Other _ -> ()  (* Mixed blocks work differently in bytecode *)

(* Check that empty arrays have tag 0 and are not mixed blocks *)
let check_empty_array_is_uniform ~array_type obj =
  let tag = Obj.tag obj in
  if tag <> 0 then
    Printf.printf "Empty %s array has tag %d, expected 0\n" array_type tag;
  assert (tag = 0);
  match Sys.backend_type with
  | Native ->
    let mixed_info = Obj.Uniform_or_mixed.of_block obj in
    begin match Obj.Uniform_or_mixed.repr mixed_info with
    | Uniform -> ()  (* Expected - empty arrays are uniform *)
    | Mixed _ -> assert false  (* Empty arrays should not be mixed *)
    end
  | Bytecode | Other _ -> ()
