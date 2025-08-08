open Compiler_owee
open! Ocaml_ir_fiber
open Ocaml_ir_common
open Std
open Sexplib.Std
open Perf_counters

module Buildid : sig
  type t

  val of_string : string -> t
  val equal : t -> t -> bool
  val find_in_cache : ?buildid_cache_dir:string -> t -> string option
end = struct
  type t = string

  let of_string t = t
  let equal a b = String.equal a b

  let get_buildid_cache_dir = function
    | Some d -> d
    | None ->
      let home = Sys.getenv_opt "HOME" in
      (match home with
       | Some h -> Filename.concat h ".debug"
       | None -> ".debug")
  ;;

  let find_in_cache ?buildid_cache_dir buildid =
    let cache = get_buildid_cache_dir buildid_cache_dir in
    match String.split_at_indices buildid ~indices:[ 2 ] with
    | [ s; e ] ->
      let ( ^/ ) = Filename.concat in
      Some (cache ^/ ".build-id" ^/ s ^/ e ^/ "elf")
    | _ -> None
  ;;
end

module Binary : sig
  module Debug_lines : sig
    type t

    val find : address:int -> t -> (string * int) option
  end

  type t

  val create : string -> t
  val buildid : t -> Buildid.t option
  val debug_lines : t -> Debug_lines.t
end = struct
  type t =
    { map : Owee_buf.t
    ; sections : Owee_elf.section array
    }

  let create path =
    let fd = Unix.openfile path [ Unix.O_RDONLY ] 0 in
    let len = Unix.lseek fd 0 Unix.SEEK_END in
    let map =
      Bigarray.array1_of_genarray
        (Unix.map_file fd Bigarray.int8_unsigned Bigarray.c_layout false [| len |])
    in
    Unix.close fd;
    let _header, sections = Owee_elf.read_elf map in
    { map; sections }
  ;;

  let buildid t =
    match Owee_elf_notes.read_buildid t.map t.sections with
    | s -> Some (Buildid.of_string s)
    | exception Owee_buf.Invalid_format _ -> None
    | exception e -> raise e
  ;;

  let fold_debug_lines ~init ~f t =
    let abbrevs = Owee_debug_abbrev.read t.sections t.map in
    let dies_comp_dir_debug_line_offsets =
      Owee_debug_info.read_all ~with_children:false ~abbrevs t.sections t.map
      |> List.fold_left
           ~f:(fun acc die ->
             match Owee_debug_info.debug_line_offset die with
             | None -> acc
             | Some offset ->
               Int.Map.add (Int64.to_int offset) (Owee_debug_info.comp_dir die) acc)
           ~init:Int.Map.empty
    in
    let get_comp_dir l =
      Int.Map.find_opt l dies_comp_dir_debug_line_offsets |> Option.bind ~f:(fun x -> x)
    in
    match Owee_elf.find_section t.sections ".debug_line" with
    | None -> init
    | Some section ->
      let body = Owee_buf.cursor (Owee_elf.section_body t.map section) in
      let pointers_to_other_sections =
        Some (Owee_elf.debug_line_pointers t.map t.sections)
      in
      let rec aux acc =
        match
          Owee_debug_line.read_chunk ~get_comp_dir body ~pointers_to_other_sections
        with
        | None -> acc
        | Some (header, chunk) ->
          let check header (state : Owee_debug_line.state) prev =
            if state.end_sequence then prev else f header state prev
          in
          aux (Owee_debug_line.fold_rows (header, chunk) check acc)
      in
      aux init
  ;;

  module Debug_lines = struct
    module Entry = struct
      type t =
        { filename : string
        ; line : int
        ; address : int
        }

      let create ~filename ~line ~address = { filename; line; address }

      let compare a1 a2 =
        let c = Int.compare a1.address a2.address in
        if c <> 0
        then c
        else (
          let c = String.compare a1.filename a2.filename in
          if c <> 0 then c else Int.compare a1.line a2.line)
      ;;
    end

    type t = Entry.t array

    let find ~address t =
      let idx_opt =
        Array.Binary_search.last_less_than_or_equal_to t ~key:address ~compare:(fun a e ->
          Int.compare a e.Entry.address)
      in
      match idx_opt with
      | None -> None
      | Some e ->
        let e = t.(e) in
        Some (e.filename, e.line)
    ;;
  end

  let debug_lines t =
    let lines =
      fold_debug_lines
        ~init:[]
        ~f:(fun header state acc ->
          let filename = Owee_debug_line.get_filename header state in
          match filename with
          | Some filename ->
            let filename =
              let prefix = "/jenga-root/" in
              if String.starts_with ~prefix filename
              then (
                let len = String.length prefix in
                String.sub ~pos:len ~len:(String.length filename - len) filename)
              else filename
            in
            Debug_lines.Entry.create ~address:state.address ~filename ~line:state.line
            :: acc
          | None -> acc)
        t
      |> Array.of_list
    in
    Array.stable_sort ~cmp:Debug_lines.Entry.compare lines;
    lines
  ;;
end

module Perf : sig
  val buildid_list : string -> (Buildid.t * string) list Result.t Fiber.t

  val find_and_open_from_buildid
    :  ?buildid_cache_dir:string
    -> Buildid.t * string
    -> Binary.t option Fiber.t
end = struct
  let buildid_list perf =
    Let_syntax.map
      (Fiber.Process.fold_lines
         ~prog:"/bin/perf"
         ~args:[ "buildid-list"; "-f"; "-i"; perf ]
         ~init:[]
         ~f:(fun acc line ->
           match String.split_on_char ~sep:' ' (String.trim line) with
           | [ buildid; path ] -> (Buildid.of_string buildid, path) :: acc
           | _ -> acc)
         ())
      ~f:(fun r ->
        match r with
        | Error e -> Result.ocaml_ir_comp_error (Error.of_exn e)
        | Ok s -> Result.return s)
  ;;

  let find_and_open_from_buildid ?buildid_cache_dir (buildid, path) =
    let exists_on_filesystem path =
      Let_syntax.map (Fiber.Sys.file_exists path) ~f:(fun status ->
        match status with
        | `Yes ->
          let binary = Binary.create path in
          if Option.map (Binary.buildid binary) ~f:(Buildid.equal buildid)
             |> Option.value ~default:false
          then Some binary
          else None
        | `No | `Unknown -> None)
    in
    Fiber.Let_syntax.Let_syntax.bind (exists_on_filesystem path) ~f:(fun binary_ok ->
      match binary_ok with
      | Some b -> Fiber.return (Some b)
      | None ->
        let in_cache = Buildid.find_in_cache ?buildid_cache_dir buildid in
        (match in_cache with
         | Some p -> exists_on_filesystem p
         | None -> Fiber.return None))
  ;;
end

let whitelist_filename = Str.regexp {|^.*\.exe$|}

let filter_out_blacklisted_buildids buildids =
  List.filter buildids ~f:(fun (_, s) -> Str.string_match whitelist_filename s 0)
;;

let select_buildid ~binary buildids =
  List.filter buildids ~f:(fun (_, s) -> String.is_substring ~substring:binary s)
;;

let find_my_buildid ?binary buildids =
  let or_error r =
    match r with
    | r :: [] -> Result.return r
    | _ ->
      Result.ocaml_ir_comp_error
        (Error.of_string
           (Format.sprintf
              "Couldn't infer the binary perf was profiling. Try calling [ocaml-ir-cli \
               perf-preprocess] while specifying [-binary]."))
  in
  match binary with
  | Some binary -> select_buildid ~binary buildids |> or_error
  | None -> filter_out_blacklisted_buildids buildids |> or_error
;;

module File = String

module Aggregated_events = struct
  module Key = struct
    type t = File.t * Event_type.t * int

    let compare (s1, e1, v1) (s2, e2, v2) =
      let c = File.compare s1 s2 in
      if c <> 0
      then c
      else (
        let c = Event_type.compare e1 e2 in
        if c <> 0 then c else Int.compare v1 v2)
    ;;
  end

  module Map = Map.Make (Key)

  let add ~line ~filename ~event t =
    Map.update
      (filename, event, line)
      (function
       | None -> Some 1
       | Some v -> Some (1 + v))
      t
  ;;
end

module Summary = struct
  type events =
    { data : Event.t list
    ; total : int
    }
  [@@deriving sexp]

  include struct
    let _ = fun (_ : events) -> ()

    let events_of_sexp =
      (let error_source__005_ = "perf_report.ml.Summary.events" in
       fun x__006_ ->
         Sexplib0.Sexp_conv_record.record_of_sexp
           ~caller:error_source__005_
           ~fields:
             (Field
                { name = "data"
                ; kind = Required
                ; conv = list_of_sexp Event.t_of_sexp
                ; rest =
                    Field
                      { name = "total"
                      ; kind = Required
                      ; conv = int_of_sexp
                      ; rest = Empty
                      }
                })
           ~index_of_field:
             (function
              | "data" -> 0
              | "total" -> 1
              | _ -> -1)
           ~allow_extra_fields:false
           ~create:(fun (data, (total, ())) : events -> { data; total })
           x__006_
        : Sexplib0.Sexp.t -> events)
    ;;

    let _ = events_of_sexp

    let sexp_of_events =
      (fun { data = data__008_; total = total__010_ } ->
         let bnds__007_ = ([] : _ Stdlib.List.t) in
         let bnds__007_ =
           let arg__011_ = sexp_of_int total__010_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "total"; arg__011_ ] :: bnds__007_
             : _ Stdlib.List.t)
         in
         let bnds__007_ =
           let arg__009_ = sexp_of_list Event.sexp_of_t data__008_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "data"; arg__009_ ] :: bnds__007_
             : _ Stdlib.List.t)
         in
         Sexplib0.Sexp.List bnds__007_
        : events -> Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_events
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  type t =
    { all_events : int Event_type.Map.t
    ; files : events Event_type.Map.t File.Map.t
    }
  [@@deriving sexp]

  include struct
    let _ = fun (_ : t) -> ()

    let t_of_sexp =
      (let error_source__013_ = "perf_report.ml.Summary.t" in
       fun x__014_ ->
         Sexplib0.Sexp_conv_record.record_of_sexp
           ~caller:error_source__013_
           ~fields:
             (Field
                { name = "all_events"
                ; kind = Required
                ; conv = Event_type.Map.t_of_sexp int_of_sexp
                ; rest =
                    Field
                      { name = "files"
                      ; kind = Required
                      ; conv =
                          File.Map.t_of_sexp (Event_type.Map.t_of_sexp events_of_sexp)
                      ; rest = Empty
                      }
                })
           ~index_of_field:
             (function
              | "all_events" -> 0
              | "files" -> 1
              | _ -> -1)
           ~allow_extra_fields:false
           ~create:(fun (all_events, (files, ())) : t -> { all_events; files })
           x__014_
        : Sexplib0.Sexp.t -> t)
    ;;

    let _ = t_of_sexp

    let sexp_of_t =
      (fun { all_events = all_events__016_; files = files__018_ } ->
         let bnds__015_ = ([] : _ Stdlib.List.t) in
         let bnds__015_ =
           let arg__019_ =
             File.Map.sexp_of_t (Event_type.Map.sexp_of_t sexp_of_events) files__018_
           in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "files"; arg__019_ ] :: bnds__015_
             : _ Stdlib.List.t)
         in
         let bnds__015_ =
           let arg__017_ = Event_type.Map.sexp_of_t sexp_of_int all_events__016_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "all_events"; arg__017_ ]
            :: bnds__015_
             : _ Stdlib.List.t)
         in
         Sexplib0.Sexp.List bnds__015_
        : t -> Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_t
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  let empty = { all_events = Event_type.Map.empty; files = File.Map.empty }

  let add_event ~filename ~event_type ~(event : Event.t) t =
    let all_events =
      Event_type.Map.update
        event_type
        (function
         | None -> Some (Event.value event)
         | Some v -> Some (v + Event.value event))
        t.all_events
    in
    let files =
      File.Map.update
        filename
        (fun o ->
          let file = Option.value o ~default:Event_type.Map.empty in
          let events =
            Event_type.Map.update
              event_type
              (function
               | None -> Some { data = [ event ]; total = Event.value event }
               | Some e ->
                 Some { data = event :: e.data; total = e.total + Event.value event })
              file
          in
          Some events)
        t.files
    in
    { all_events; files }
  ;;

  let of_aggregated_events aggregated =
    Aggregated_events.Map.fold
      (fun (filename, event_type, line) count t ->
        add_event
          ~filename
          ~event_type
          ~event:(Event.create ~line ~col:0 ~discriminator:0 count)
          t)
      aggregated
      empty
  ;;
end

let rec trim_colon s =
  if s = ""
  then s
  else if Char.equal (String.unsafe_get s (String.length s - 1)) ':'
  then trim_colon (String.sub ~pos:0 ~len:(String.length s - 1) s)
  else s
;;

let parse_line ~target_dso ~debug_lines acc line =
  let line = String.trim line in
  let atoms =
    String.split_on_char ~sep:' ' line
    |> List.filter ~f:(fun s -> String.compare s "" <> 0)
  in
  match atoms with
  | event :: address :: dso :: _ ->
    let dso = String.sub dso ~pos:1 ~len:(String.length dso - 2) in
    if String.equal dso target_dso
    then (
      let event = String.trim event in
      let event = Event_type.of_string (trim_colon event) in
      let address = int_of_string ("0x" ^ address) in
      match Binary.Debug_lines.find ~address debug_lines with
      | Some (filename, line) -> Aggregated_events.add ~line ~filename ~event acc
      | None -> acc)
    else acc
  | _ -> acc
;;

let perf_script ~f ~init perf_data =
  Let_syntax.map
    (Fiber.Process.fold_lines
       ~prog:"/bin/perf"
       ~args:[ "script"; "-G"; "-F"; "ip,event,dso,brstack"; "-i"; perf_data ]
       ~init
       ~f
       ())
    ~f:(fun r ->
      match r with
      | Error exn -> Result.ocaml_ir_comp_error (Std.Error.of_exn exn)
      | Ok s -> Result.return s)
;;

type summary = Summary.t

let summary_of_sexp = Summary.t_of_sexp
let sexp_of_summary = Summary.sexp_of_t

let from_perf_script ?buildid_cache_dir ?binary perf_data =
  Result.Fiber.Let_syntax.Let_syntax.bind
    (Perf.buildid_list perf_data)
    ~f:(fun buildids ->
    Result.Fiber.Let_syntax.Let_syntax.bind
      (find_my_buildid ?binary buildids |> Fiber.return)
      ~f:(fun my_buildid ->
        Result.Fiber.Let_syntax.Let_syntax.bind
          (Fiber.Let_syntax.Let_syntax.bind
             (Perf.find_and_open_from_buildid ?buildid_cache_dir my_buildid)
             ~f:(fun binary_opt ->
             match binary_opt with
             | Some binary -> Result.Fiber.return binary
             | None ->
               Fiber.return
                 (Result.ocaml_ir_comp_error
                    (Error.of_string
                       (Format.sprintf
                          "Couldn't load binary %s.\n\
                           Perf can only find binaries that:\n\
                           - Are on the same absolute path as when perf was executed\n\
                           - Were added to the build-id cache either by executing [perf \
                           buildid-cache -a <binary>] or by running [perf-archive] on \
                           after [perf-record] and then [tar xvf [perf-archive.tar.bz2] \
                           -C ~/.debug]"
                          (snd my_buildid))))))
          ~f:(fun binary ->
            let debug_lines = Binary.debug_lines binary in
            let target_dso = snd my_buildid in
            Result.Fiber.Let_syntax.Let_syntax.map
              (perf_script
                 ~init:Aggregated_events.Map.empty
                 ~f:(parse_line ~target_dso ~debug_lines)
                 perf_data)
              ~f:(fun r -> Summary.of_aggregated_events r))))
;;

let version = 1

let fast_file_hash perf_data =
  let stats = Unix.stat perf_data in
  "ocaml-ir-perf-" ^ Digest.to_hex (Digest.string (Marshal.to_string (stats, version) []))
;;

let force_create_summary ?buildid_cache_dir ?binary ~perf_data cached_path =
  Result.Fiber.Let_syntax.Let_syntax.bind
    (from_perf_script ?buildid_cache_dir ?binary perf_data)
    ~f:(fun summary ->
    Result.Fiber.Let_syntax.Let_syntax.map
      (Fiber.Io.create_file_write_content
         ~path:cached_path
         ~content:(Sexplib.Sexp.to_string (Summary.sexp_of_t summary))
      |> Result.Fiber.ok)
      ~f:(fun () -> summary))
;;

let summarize_perf_data ?(force_recompute = false) ?buildid_cache_dir ?binary perf_data =
  let cached_path = Filename.concat "/tmp" (fast_file_hash perf_data) in
  Result.Fiber.Let_syntax.Let_syntax.bind
    (Result.Fiber.ok (Fiber.Sys.file_exists cached_path))
    ~f:(fun exists ->
      if force_recompute || exists <> `Yes
      then force_create_summary ?buildid_cache_dir ?binary ~perf_data cached_path
      else
        Result.Fiber.Let_syntax.Let_syntax.bind
          (Result.Fiber.ok (Fiber.Io.read_file ~path:cached_path))
          ~f:(fun c ->
            try Result.Fiber.return (Summary.t_of_sexp (Sexplib.Sexp.of_string c)) with
            | _ ->
              Sys.remove cached_path;
              force_create_summary ?buildid_cache_dir ?binary ~perf_data cached_path))
;;

let rec remove_workspace filename =
  let dirname = Filename.dirname filename in
  let basename = Filename.basename filename in
  match dirname with
  | "." | "/" -> filename
  | _ -> Filename.concat (remove_workspace dirname) basename
;;

let events_in_file ~filename ~event Summary.{ all_events; files } =
  let filename = remove_workspace filename in
  let total_in_project =
    Event_type.Map.find_opt event all_events |> Option.value ~default:0
  in
  let total_in_file, values =
    match File.Map.find_opt filename files with
    | Some events ->
      (match Event_type.Map.find_opt event events with
       | None -> 0, []
       | Some Summary.{ total; data } -> total, data)
    | None -> 0, []
  in
  Events_for_a_file.create ~values ~filename ~event ~total_in_file ~total_in_project
;;

let event_types_in_project Summary.{ all_events; _ } =
  Event_types_in_project.of_list
    (Event_type.Map.to_seq all_events |> Seq.map fst |> List.of_seq)
;;

let event_count_per_files ~event Summary.{ files; _ } =
  File.Map.fold
    (fun filename events acc ->
      match Event_type.Map.find_opt event events with
      | None -> acc
      | Some Summary.{ total; _ } -> (filename, total) :: acc)
    files
    []
  |> Event_count_per_files.of_list
;;
