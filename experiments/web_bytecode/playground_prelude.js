export const playgroundPreludeSource = `
module Multicore : sig
  val max_domains : unit -> int
  val current_domain : unit -> int
  type 'a spawn_result =
    | Spawned
    | Failed of 'a * exn * Printexc.raw_backtrace
  val spawn_on
    :   domain:int
    -> ('a @ contended once portable unique -> unit) @ once portable unyielding
    -> 'a @ contended once portable unique
    -> 'a spawn_result
  val spawn
    :  ('a @ contended once portable unique -> unit) @ once portable unyielding
    -> 'a @ contended once portable unique
    -> 'a spawn_result
end = struct
  let max_domains () = 1
  let current_domain () = 0
  type 'a spawn_result =
    | Spawned
    | Failed of 'a * exn * Printexc.raw_backtrace
  let spawn_on ~domain:_ f x =
    f x;
    Spawned
  let spawn f x =
    spawn_on ~domain:0 f x
end

`;

function escapedStringLiteral(text) {
  return String(text).replace(/\\/g, "\\\\").replace(/"/g, '\\"');
}

export function withPlaygroundPrelude(filename, source) {
  return `${playgroundPreludeSource}# 1 "${escapedStringLiteral(filename)}"\n${source}`;
}

export function stripPlaygroundPreludeInterface(output) {
  return String(output).replace(/^module Multicore :\n\s*sig\n(?:.|\n)*?\n\s*end(?: @@ stateless)?\n?/, "");
}
