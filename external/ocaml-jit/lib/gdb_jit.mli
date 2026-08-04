(** GDB JIT interface registration.

    Publishes an in-memory ELF symfile to GDB via the protocol described at
    https://sourceware.org/gdb/current/onlinedocs/gdb.html/JIT-Interface.html.

    The implementation uses globals [__jit_debug_descriptor] and
    [__jit_debug_register_code] in [jit_stubs.c]; GDB looks these up by name
    in the inferior. *)

type handle

(** Register an ELF symfile contained in the given buffer with GDB. The
    underlying bytes of [symfile] must remain valid for the lifetime of the
    returned handle; this is achieved by keeping a reference to the buffer
    inside the handle. *)
val register : Compiler_owee.Owee_buf.t -> handle

(** Unregister a previously registered symfile. After this returns, the
    handle's buffer can be collected. *)
val unregister : handle -> unit
