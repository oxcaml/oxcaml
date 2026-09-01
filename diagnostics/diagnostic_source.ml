type t =
  { file : string;
    text : string
  }

let create ~file ~text = { file; text }

let load (loc : Location.t) =
  let file = loc.loc_start.pos_fname in
  let text =
    match In_channel.with_open_bin file In_channel.input_all with
    | text -> text
    | exception Sys_error _ -> ""
  in
  create ~file ~text

let holds t (loc : Location.t) =
  (not (Location.is_none loc)) && String.equal loc.loc_start.pos_fname t.file

let length t = String.length t.text

let sub t ~pos ~len = String.sub t.text pos len
