(* TEST
 modules = "block_checks.ml";
 {
   bytecode;
 } {
   native;
 }
*)

type target =
  | Broadcast
  | Entity of int32_u * int32_u
  | Topic of int32_u * int32_u
[@@unboxed]

external box_int32 : int32_u -> int32 = "%box_int32"

external get : target array -> int -> target = "%array_safe_get"

let[@inline never] read_target (targets : target array) i =
  match get targets i with
  | Broadcast -> None
  | Entity (id, generation) ->
    Some (`Entity, box_int32 id, box_int32 generation)
  | Topic (id, generation) ->
    Some (`Topic, box_int32 id, box_int32 generation)

let () =
  let calls = ref 0 in
  let[@inline never] component x = incr calls; (x : int32_u) in
  let targets =
    [| Entity (component #13l, component #21l); Broadcast; Topic (#13l, #21l) |]
  in
  assert (!calls = 2);
  assert (read_target targets 0 = Some (`Entity, 13l, 21l));
  assert (read_target targets 1 = None);
  assert (read_target targets 2 = Some (`Topic, 13l, 21l));
  Block_checks.check_mixed_block_scannable_size
    ~array_type:"target" (Obj.repr targets) 0
