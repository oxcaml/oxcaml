open Lambda

(** "Fractures" tlambda (the result of transl) into slambda.

    This process conceptually splits ~all constructs into their compiletime and
    runtime halves and composes them so that the resulting slambda will evaluate
    to an [SLhalves] where the compile time half is the compile-time
    representation of whatever the initial tlambda evalutaes to and the runtime
    half is lambda that evaluates to theruntime representation of it. *)
val fracture : lambda -> slambda
