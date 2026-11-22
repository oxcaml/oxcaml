(* TEST
 {
   bytecode;
 } {
   native;
 }
*)

type payload = Text of string | Bits of int64_u [@@unboxed]
type envelope = { payload : payload }

external box_int64 : int64_u -> int64 = "%box_int64"

let[@inline never] text c = { payload = Text (String.make 3 c) }
let[@inline never] bits n = { payload = Bits n }

let[@inline never] read envelope =
  match envelope.payload with
  | Text s -> `Text s
  | Bits n -> `Bits (box_int64 n)

let () =
  let a = text 'x' in
  let b = bits #42L in
  Gc.minor ();
  Gc.compact ();
  assert (read a = `Text "xxx");
  assert (read b = `Bits 42L);
  match Sys.backend_type with
  | Native ->
    let block = Obj.repr a in
    begin match Obj.Uniform_or_mixed.(repr (of_block block)) with
    | Uniform -> assert false
    | Mixed { scannable_prefix_len } ->
      assert (scannable_prefix_len > 0);
      assert (scannable_prefix_len < Obj.size block);
      let inactive = Obj.repr b in
      for i = 0 to scannable_prefix_len - 1 do
        assert (Obj.is_int (Obj.field inactive i))
      done
    end
  | Bytecode | Other _ -> ()
