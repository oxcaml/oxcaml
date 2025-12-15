type t =
  { mutable buffer : Buffer.t;
    offsets : (string, int) Hashtbl.t
  }

let create () =
  let buffer = Buffer.create 256 in
  (* ELF string tables start with a null byte *)
  Buffer.add_char buffer '\x00';
  { buffer; offsets = Hashtbl.create 64 }

let add t s =
  match Hashtbl.find_opt t.offsets s with
  | Some offset -> offset
  | None ->
    let offset = Buffer.length t.buffer in
    Buffer.add_string t.buffer s;
    Buffer.add_char t.buffer '\x00';
    Hashtbl.add t.offsets s offset;
    offset

let length t = Buffer.length t.buffer

let contents t = Buffer.to_bytes t.buffer
