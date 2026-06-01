export const playgroundPreludeSource = `
module Backtrace : sig
  type t = Printexc.raw_backtrace
end = struct
  type t = Printexc.raw_backtrace
end

module Or_canceled : sig
  type 'a t = 'a
end = struct
  type 'a t = 'a
end

module Cancellation : sig
  type t
end = struct
  type t = unit
end

module Multicore : sig
  val max_domains : unit -> int
  val current_domain : unit -> int
  type 'a spawn_result =
    | Spawned
    | Failed of 'a * exn * Printexc.raw_backtrace
  val spawn_on
    :   domain:int
    -> f:('a -> unit) @ portable
    -> 'a
    -> 'a spawn_result
  val spawn
    :  f:('a -> unit) @ portable
    -> 'a
    -> 'a spawn_result
end = struct
  let max_domains () = 1
  let current_domain () = 0
  type 'a spawn_result =
    | Spawned
    | Failed of 'a * exn * Printexc.raw_backtrace
  let spawn_on ~domain:_ ~f x =
    f x;
    Spawned
  let spawn ~f x =
    spawn_on ~domain:0 ~f x
end

module Concurrent : sig
  type 'a spawn_result =
    | Spawned
    | Failed of 'a * exn * Backtrace.t
  type 'concurrent_ctx t
  module Scope : sig
    type 'scope_ctx t
    val context : 'scope_ctx t @ local -> 'scope_ctx @ contended local portable
  end
  type ('scope_ctx, 'concurrent_ctx) spawn
  val sequential : unit t @@ portable
  val with_scope
    :  'concurrent_ctx t @ local
    -> 'scope_ctx @ portable
    -> f:(('scope_ctx, 'concurrent_ctx) spawn @ local -> 'r) @ local once unyielding
    -> 'r
  val spawn
    :  ('scope_ctx, 'concurrent_ctx) spawn @ local
    -> f:
         ('scope_ctx Scope.t @ local
          -> 'concurrent_ctx @ local
          -> 'concurrent_ctx t @ local portable
          -> unit)
       @ once portable
    -> unit
  val spawn_with
    :  ('scope_ctx, 'concurrent_ctx) spawn @ local
    -> f:
         ('scope_ctx Scope.t @ local
          -> 'concurrent_ctx @ local
          -> 'concurrent_ctx t @ local portable
          -> 'resource @ contended once portable unique
          -> unit)
       @ once portable
    -> 'resource @ contended once portable unique
    -> 'resource spawn_result @ contended once portable unique
  val spawn_daemon
    :  ('scope_ctx, 'concurrent_ctx) spawn @ local
    -> f:
         ('scope_ctx Scope.t @ local
          -> Cancellation.t @ local
          -> 'concurrent_ctx @ local
          -> 'concurrent_ctx t @ local portable
          -> unit Or_canceled.t)
       @ once portable
    -> unit
  val spawn_daemon'
    :  ('scope_ctx, 'concurrent_ctx) spawn @ local
    -> f:
         ('scope_ctx Scope.t @ local
          -> Cancellation.t @ local
          -> 'concurrent_ctx @ local
          -> 'concurrent_ctx t @ local portable
          -> unit)
       @ once portable
    -> unit
end = struct
  type 'a spawn_result =
    | Spawned
    | Failed of 'a * exn * Backtrace.t
  type 'concurrent_ctx t = unit
  module Scope = struct
    type 'scope_ctx t = 'scope_ctx
    external context
      : 'scope_ctx t @ local -> 'scope_ctx @ contended local portable
      = "%identity"
  end
  type ('scope_ctx, 'concurrent_ctx) spawn =
    { scope : 'scope_ctx Scope.t }
  let (sequential @ portable) = ()
  let dummy_context () = Obj.magic ()
  let with_scope
      (concurrent : 'concurrent_ctx t @ local)
      (scope : 'scope_ctx @ portable)
      ~(f : (('scope_ctx, 'concurrent_ctx) spawn @ local -> 'r) @ local once unyielding)
    : 'r =
    ignore concurrent;
    f { scope }
  let spawn { scope } ~f =
    f scope (dummy_context ()) sequential
  let spawn_with spawn_handle ~f resource =
    spawn spawn_handle ~f:(fun scope concurrent concurrent_t ->
      f scope concurrent concurrent_t resource);
    Spawned
  let dummy_cancellation () : Cancellation.t @ local = Obj.magic ()
  let spawn_daemon spawn_handle ~f =
    spawn spawn_handle ~f:(fun scope concurrent concurrent_t ->
      let (_ : unit Or_canceled.t) =
        f scope (dummy_cancellation ()) concurrent concurrent_t
      in
      ())
  let spawn_daemon' spawn_handle ~f =
    spawn spawn_handle ~f:(fun scope concurrent concurrent_t ->
      let (_ : unit) = f scope (dummy_cancellation ()) concurrent concurrent_t in
      ())
end

module Terminator : sig
  type t
  val unkillable : t
end = struct
  type t = unit
  let unkillable = ()
end

module Await : sig
  module Terminator : sig
    type t = Terminator.t
    val unkillable : t
  end
end = struct
  module Terminator = Terminator
end

module Concurrent_in_thread : sig
  val with_blocking
    :  Terminator.t @ local
    -> f:(unit Concurrent.t @ local portable -> 'r) @ local once
    -> 'r
end = struct
  let with_blocking
      (_ : Terminator.t @ local)
      ~(f : (unit Concurrent.t @ local portable -> 'r) @ local once)
    : 'r =
    f Concurrent.sequential
end

module Parallel : sig
  type t
  val fork_join2
    :  t @ local
    -> (t @ local -> 'a) @ shareable
    -> (t @ local -> 'b) @ shareable
    -> #('a * 'b)
  module Scheduler : sig
    module Sequential : sig
      type scheduler
      val create : unit -> scheduler
      val parallel : scheduler -> f:(t @ local -> 'a) -> 'a
    end
  end
end = struct
  type t = unit
  let fork_join2 parallel left right =
    #(left parallel, right parallel)
  module Scheduler = struct
    module Sequential = struct
      type scheduler = unit
      let create () = ()
      let parallel scheduler ~f =
        f scheduler
    end
  end
end

module Toy : sig
  val fork_join2
    :  (unit -> 'a) @ shareable
    -> (unit -> 'b) @ shareable
    -> 'a * 'b
  val spawn : (unit -> unit) @ portable -> unit
end = struct
  let fork_join2 left right =
    (left (), right ())
  let spawn f =
    f ()
end

`;

function escapedStringLiteral(text) {
  return String(text).replace(/\\/g, "\\\\").replace(/"/g, '\\"');
}

export function withPlaygroundPrelude(filename, source) {
  return `${playgroundPreludeSource}# 1 "${escapedStringLiteral(filename)}"\n${source}`;
}

export function playgroundPreludeOffset(filename) {
  return withPlaygroundPrelude(filename, "").length;
}

function stripTopLevelModuleInterface(output, moduleName) {
  const lines = String(output).split("\n");
  const kept = [];
  let skipping = false;
  let depth = 0;
  const start = new RegExp(`^module ${moduleName} :`);

  for (const line of lines) {
    if (!skipping && start.test(line)) {
      skipping = true;
      depth = 0;
    }

    if (skipping) {
      const sigs = line.match(/\bsig\b/g)?.length ?? 0;
      const ends = line.match(/\bend\b/g)?.length ?? 0;
      depth += sigs - ends;
      if (depth <= 0 && ends > 0) {
        skipping = false;
      }
    } else {
      kept.push(line);
    }
  }

  return kept.join("\n");
}

export function stripPlaygroundPreludeInterface(output) {
  let stripped = String(output);
  for (const moduleName of [
    "Backtrace",
    "Or_canceled",
    "Cancellation",
    "Multicore",
    "Concurrent",
    "Terminator",
    "Await",
    "Concurrent_in_thread",
    "Parallel",
    "Toy",
  ]) {
    stripped = stripTopLevelModuleInterface(stripped, moduleName);
  }
  return stripped;
}
