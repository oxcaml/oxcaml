let { Logger.log } = Logger.for_section "Phase cache"

module type S = sig
  type t
  type output

  val f : t -> output

  val title : string

  module Fingerprint : sig
    type input
    type t

    val make : input -> (t, string) result
    val equal : t -> t -> bool
  end
  with type input := t
end

module With_cache (Phase : S) = struct
  module Version = struct
    type t = int option

    let equal v1 v2 =
      match (v1, v2) with
      | None, _ | _, None -> false
      | Some v1, Some v2 -> Int.equal v1 v2
  end

  type t = { output : Phase.output; cache_was_hit : bool; version : Version.t }
  type cache =
    { fingerprint : Phase.Fingerprint.t;
      output : Phase.output;
      version : Version.t
    }

  let get_next_version =
    (* A mutable counter separate from [cache.version] is necessary because of [Phase.f]
       erroring. In [apply], when [Phase.f = Error], cache is reset to [None], losing the
       value of [next_version] if not stored elsewhere. *)
    let next_version = ref 0 in
    fun () : Version.t ->
      let v = Some !next_version in
      incr next_version;
      v

  let cache = ref None

  let apply ?(cache_disabling = None) ?(force_invalidation = false) input =
    let title = Phase.title in
    match cache_disabling with
    | Some reason ->
      log ~title "Cache is disabled: %s" reason;
      cache := None;
      let output = Phase.f input in
      { output; cache_was_hit = false; version = None }
    | None -> (
      let new_fingerprint = Phase.Fingerprint.make input in
      match (!cache, new_fingerprint) with
      | None, Ok new_fingerprint ->
        log ~title "Cache wasn't populated\n";
        let output = Phase.f input in
        let version = get_next_version () in
        cache := Some { fingerprint = new_fingerprint; output; version };
        { output; cache_was_hit = false; version }
      | Some { fingerprint; output; version = old_version }, Ok new_fingerprint
        ->
        if
          (not force_invalidation)
          && Phase.Fingerprint.equal fingerprint new_fingerprint
        then (
          log ~title "Cache hit";
          { output; cache_was_hit = true; version = old_version })
        else (
          log ~title "Cache invalidation";
          let output = Phase.f input in
          let version = get_next_version () in
          cache := Some { fingerprint = new_fingerprint; output; version };
          { output; cache_was_hit = false; version })
      | (None | Some _), Error err ->
        log ~title "Cache workflow is incomplete: %s" err;
        cache := None;
        let output = Phase.f input in
        { output; cache_was_hit = false; version = None })
end
