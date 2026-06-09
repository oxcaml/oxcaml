type handle =
  { (* The underlying ELF buffer. GDB reads from this memory directly via the
       symfile_addr in the C-side jit_code_entry, so the Bigarray header must
       stay live so the off-heap data is not freed. *)
    symfile : Compiler_owee.Owee_buf.t;
    (* Address of the C-allocated jit_code_entry, returned by the register
       stub. Used as a token for unregistration. *)
    entry : nativeint
  }

external register_c : Compiler_owee.Owee_buf.t -> nativeint
  = "caml_jit_gdb_register"

external unregister_c : nativeint -> unit = "caml_jit_gdb_unregister"

let register symfile =
  let entry = register_c symfile in
  { symfile; entry }

let unregister h =
  unregister_c h.entry;
  (* Keep [h.symfile] reachable until after the C call. Reading the field
     here prevents an over-eager GC from freeing the buffer between the C
     call's read of [symfile_addr] and the unregister notification. *)
  ignore (Bigarray.Array1.dim h.symfile : int)
