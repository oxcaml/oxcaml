module Change = struct
  module type S = sig
    type t

    val undo : t -> unit
  end
end

module type S = sig
  module Change : Change.S

  val set_log : (Change.t -> unit) -> unit
end

module Make (C : Change.S) : sig
  include S with module Change := C

  (** [log change] reports [change] to the log installed by [set_log]. *)
  val log : C.t -> unit
end = struct
  let global_log : (C.t -> unit) ref = ref (fun _ -> ())

  let log change = !global_log change

  let set_log log = global_log := log
end
