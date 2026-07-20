module Mode = struct
  type t =
    | Soundness
    | Completeness

  let to_string = function
    | Soundness -> "soundness"
    | Completeness -> "completeness"

  let of_string = function
    | "soundness" -> Some Soundness
    | "completeness" -> Some Completeness
    | _ -> None
end

module Sample = struct
  type t =
    { fe_source : string;
      be_source : string
    }

  let to_string { fe_source; be_source } =
    Printf.sprintf "(* frontend *)\n%s(* backend *)\n%s" fe_source be_source
end

let prelude = ""

(* CR shsong: placeholder generator. Emits a trivial non-allocating identity so
   the end-to-end oracle path (milestone 1) can be exercised; it always lands in
   the [Agree_noalloc] quadrant. Replace with real type-directed generation
   (milestones 2-4): build a well-typed body of a chosen type, tracking a coarse
   allocation potential to steer toward the [mode]'s target quadrant. *)
let generate ~mode ~seed =
  ignore (mode : Mode.t);
  ignore (seed : int);
  let body = "x0" in
  let fe_source =
    Printf.sprintf "%slet (f @ noalloc_strict) (x0 : int) = %s\n" prelude body
  in
  let be_source =
    Printf.sprintf "%slet[@zero_alloc strict] f (x0 : int) = %s\n" prelude body
  in
  { Sample.fe_source; be_source }
