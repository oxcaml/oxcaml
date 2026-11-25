(* TEST
 modules = "cstubs.c";
 flags = "-runtime-variant=d";
 stack-allocation;
 native;
*)

(* Model Mem.alloc_stack: allocate local bytes, then return only their unmanaged
   address into the caller's region. The bytes are not kept alive as a GC root. *)
module Mem = struct
  type t : bits64 mod external_ many stateless unyielding

  external create_bytes : nativeint_u -> bytes @ local
    = "" "caml_alloc_local_string"
    [@@noalloc]

  external of_bytes : bytes @ local -> t @ local
    = "" "caml_native_pointer_of_value"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  let[@inline] alloc_stack ~len = exclave_
    let bytes = create_bytes len in
    of_bytes bytes

  external get : t @ local -> int64_u = "%unsafe_get_ext_ptr"
  external set : t @ local -> int64_u -> unit = "%unsafe_set_ext_ptr"
end

external box_int64 : int64_u -> int64 = "%box_int64"

let[@inline never] check (pointer : Mem.t @ local) =
  Gc.minor ();
  assert (box_int64 (Mem.get pointer) = 42L);
  Gc.full_major ();
  assert (box_int64 (Mem.get pointer) = 42L)

let () =
  let pointer = Mem.alloc_stack ~len:#8n in
  Mem.set pointer #42L;
  check pointer [@nontail]
