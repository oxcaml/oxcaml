(* Minimal stand-in for the compiler's [Location] module, enough for the copies
   of [Parsetree]/[Asttypes]/[Longident]/[Pprintast] linked into the standard
   library for runtime metaprogramming. The real [Location] cannot be copied
   here: it depends on [Clflags]/[Config], which are compiler-only. Only the
   location TYPE and a few accessors are needed (generated quotes carry no source
   locations), and the layout matches the compiler's [Location.t]. *)

type t = {
  loc_start : Lexing.position;
  loc_end : Lexing.position;
  loc_ghost : bool;
}

type 'a loc = {
  txt : 'a;
  loc : t;
}

let none =
  let pos = Lexing.dummy_pos in
  { loc_start = pos; loc_end = pos; loc_ghost = true }

let mkloc txt loc = { txt; loc }
let mknoloc txt = mkloc txt none
let get_txt { txt; _ } = txt
