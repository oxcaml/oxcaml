(* Reading consecutive LBR samples from a Linux perf profile (perf record -j
   any,u), via "perf script -F [pid,]period,ip,brstack". Each sample is printed
   as the period, the ip and the branch stack entries on one line, or, when the
   profile has call chains (perf record --call-graph), as the period alone on a
   line, then one tab-indented line per call chain frame (the ip first, then the
   return addresses up the stack), then the branch stack entries on a line. The
   ip and the frames are hexadecimal without prefix; an entry is
   <from>/<to>/<flags...> (the flags vary with the perf version and are
   ignored); entries are most recent first, one per recorded taken branch.

   Every entry says that a taken branch went from <from> to <to>; every pair of
   consecutive entries that the code from the older one's target up to the newer
   one's source (a range that may be that single branch instruction) executed
   sequentially. The ip is not used for decoding: only Intel's LBR (and the
   runtime's single-stepping emulator, which produces the same format) freeze
   the branch stack at the sampled instruction; on AMD's the ip and the branch
   stack are not synchronized (the ip is often below the most recent target or
   megabytes above it). The call chains are checked for shape but not used.

   Runtime versus link-time addresses: perf samples the addresses the code ran
   at, the FDO metadata describes link-time addresses, and the two differ for a
   position-independent executable (loaded at an address chosen at run time,
   separately in every process). When the executable is given, the profile is
   read with its mmap and task events (perf script --show-mmap-events
   --show-task-events, with the pid as the first field of every line), which say
   where each process mapped which part of the file, and every sampled address
   is translated to the link-time address of the same byte of the executable;
   addresses in other code (the C library, the kernel) become [foreign], which
   no metadata matches. Without the executable, addresses are taken as they are:
   the runtime's emulator prints link-time addresses.

   Samples are delivered without aggregating or reordering their branches:
   metadata interpretation needs the execution order to maintain call stacks. *)

type sample =
  { count : int64;
    ip : int64 option;
    branches : (int64 * int64) list
  }

type executable =
  { path : string;
    address_of_offset : int64 -> int64 option
  }

let foreign = -1L

(* A mapping of part of the executable into a process: the bytes from file
   offset [pgoff] are at runtime addresses [start, start + size). *)
type mapping =
  { start : int64;
    size : int64;
    pgoff : int64
  }

(* The mappings of the executable in every process seen so far, by pid. *)
module Pid_tbl = Hashtbl.Make (Int)

type processes = mapping list Pid_tbl.t

let translate executable (processes : processes) ~pid address =
  let mappings = Option.value (Pid_tbl.find_opt processes pid) ~default:[] in
  let translated =
    List.find_map
      (fun { start; size; pgoff } ->
        if
          Int64.compare start address <= 0
          && Int64.compare address (Int64.add start size) < 0
        then
          executable.address_of_offset
            (Int64.add pgoff (Int64.sub address start))
        else None)
      mappings
  in
  Option.value translated ~default:foreign

(* The paths perf may name the executable by: as given, and resolved. *)
let executable_paths executable =
  match Unix.realpath executable.path with
  | realpath -> [executable.path; realpath]
  | exception Unix.Unix_error _ -> [executable.path]

let split_nonempty c s =
  String.split_on_char c s |> List.filter (fun s -> not (String.equal s ""))

(* The suffix of [s] starting at the first occurrence of [sub], if any. *)
let suffix_from ~sub s =
  let n = String.length sub and len = String.length s in
  let rec go i =
    if i + n > len
    then None
    else if String.equal (String.sub s i n) sub
    then Some (String.sub s i (len - i))
    else go (i + 1)
  in
  go 0

(* An event line (perf script --show-mmap-events --show-task-events), without
   its leading pid:

   PERF_RECORD_MMAP2 pid/tid: [0xstart(0xsize) @ 0xpgoff ...]: perms path
   PERF_RECORD_MMAP pid/tid: [0xstart(0xsize) @ 0xpgoff]: perms path
   PERF_RECORD_FORK(pid:tid):(ppid:ptid) PERF_RECORD_COMM exec: name:pid/tid

   The others (COMM without exec, EXIT, ...) carry nothing needed here. *)
let handle_event ~paths (processes : processes) line =
  let fail () = failwith (Printf.sprintf "cannot parse perf event %S" line) in
  let int64 s =
    match Int64.of_string_opt s with Some n -> n | None -> fail ()
  in
  let int s = match int_of_string_opt s with Some n -> n | None -> fail () in
  (* The pid of "pid/tid" or "pid:tid". *)
  let pid_of s =
    match split_nonempty '/' s @ split_nonempty ':' s with
    | pid :: _ -> int pid
    | [] -> fail ()
  in
  let after prefix =
    if String.starts_with ~prefix line
    then
      Some
        (String.sub line (String.length prefix)
           (String.length line - String.length prefix))
    else None
  in
  let mmap rest =
    match String.index_opt rest '[', String.index_opt rest ']' with
    | Some lb, Some rb when lb < rb -> (
      let pid = pid_of (String.trim (String.sub rest 0 lb)) in
      let inside = String.sub rest (lb + 1) (rb - lb - 1) in
      let tail = String.sub rest (rb + 1) (String.length rest - rb - 1) in
      let range =
        match String.index_opt inside '(', String.index_opt inside ')' with
        | Some lp, Some rp when lp < rp -> (
          let start = int64 (String.sub inside 0 lp) in
          let size = int64 (String.sub inside (lp + 1) (rp - lp - 1)) in
          match
            split_nonempty ' '
              (String.sub inside (rp + 1) (String.length inside - rp - 1))
          with
          | "@" :: pgoff :: _ -> { start; size; pgoff = int64 pgoff }
          | _ -> fail ())
        | _ -> fail ()
      in
      (* ": perms path" *)
      match split_nonempty ' ' tail with
      | ":" :: perms :: path ->
        let path = String.concat " " path in
        if String.contains perms 'x' && List.mem path paths
        then
          Pid_tbl.replace processes pid
            (range :: Option.value (Pid_tbl.find_opt processes pid) ~default:[])
      | _ -> fail ())
    | _ -> fail ()
  in
  match after "PERF_RECORD_MMAP2 " with
  | Some rest -> mmap rest
  | None -> (
    match after "PERF_RECORD_MMAP " with
    | Some rest -> mmap rest
    | None -> (
      match after "PERF_RECORD_FORK" with
      | Some rest -> (
        (* The child inherits the parent's mappings (until it execs). *)
        match
          split_nonempty ':'
            (String.map (fun c -> if c = '(' || c = ')' then ' ' else c) rest)
        with
        | child :: _ :: parent :: _ ->
          let child = int (String.trim child)
          and parent = int (String.trim parent) in
          if child <> parent
          then
            Option.iter
              (fun mappings -> Pid_tbl.replace processes child mappings)
              (Pid_tbl.find_opt processes parent)
        | _ -> fail ())
      | None -> (
        match after "PERF_RECORD_COMM exec: " with
        | Some rest -> (
          (* A new image: its mappings follow, the old ones are gone. *)
          match String.rindex_opt rest ':' with
          | Some i ->
            Pid_tbl.remove processes
              (pid_of (String.sub rest (i + 1) (String.length rest - i - 1)))
          | None -> fail ())
        | None -> ())))

let iter_channel ?executable ic ~f =
  let processes : processes = Pid_tbl.create 16 in
  let paths = Option.fold ~none:[] ~some:executable_paths executable in
  let translate =
    match executable with
    | None -> fun ~pid:_ address -> address
    | Some executable -> translate executable processes
  in
  (* The pid, period and ip of a sample whose branch stack is still to come (the
     call chain frames are in between). *)
  let pending = ref None in
  let rec loop () =
    match In_channel.input_line ic with
    | None -> ()
    | Some line ->
      let fail () = failwith (Printf.sprintf "cannot parse sample %S" line) in
      let int64 s =
        match Int64.of_string_opt s with Some n -> n | None -> fail ()
      in
      let hex s = int64 ("0x" ^ s) in
      let is_entry s = String.contains s '/' in
      let tokens = split_nonempty ' ' (String.trim line) in
      (* With the executable, every line starts with the pid. *)
      let pid, tokens =
        match executable, tokens with
        | None, tokens -> 0, tokens
        | Some _, pid :: tokens when not (is_entry pid) -> (
          match int_of_string_opt pid with
          | Some pid -> pid, tokens
          | None -> fail ())
        | Some _, ([] | _ :: _) -> fail ()
      in
      let entry token =
        match String.split_on_char '/' token with
        | source :: target :: _ -> int64 source, int64 target
        | [_] | [] -> fail ()
      in
      let sample ~pid ~count ~ip ~branches =
        f
          { count;
            ip = Option.map (translate ~pid) ip;
            branches =
              List.map
                (fun token ->
                  let source, target = entry token in
                  translate ~pid source, translate ~pid target)
                branches
          }
      in
      (match tokens with
      | [] -> ()
      | event :: _ when String.starts_with ~prefix:"PERF_RECORD_" event -> (
        match suffix_from ~sub:"PERF_RECORD_" line with
        | Some event -> handle_event ~paths processes event
        | None -> fail ())
      | [frame]
        when (not (is_entry frame)) && String.starts_with ~prefix:"\t" line -> (
        (* A call chain frame: the first one is the ip. *)
        match !pending with
        | None -> fail ()
        | Some (pid, period, None) ->
          pending := Some (pid, period, Some (hex frame))
        | Some (_, _, Some _) -> ignore (hex frame))
      | [period] when not (is_entry period) ->
        (* A sample with call chain: its frames and branch stack follow. (An old
           perf printing an instruction profile has no branch stacks at all.) *)
        pending := Some (pid, int64 period, None)
      | first :: rest when not (is_entry first) ->
        let count = int64 first in
        let ip, branches =
          match rest with
          | ip :: rest when not (is_entry ip) -> Some (hex ip), rest
          | rest -> None, rest
        in
        pending := None;
        sample ~pid ~count ~ip ~branches
      | entries ->
        let pid, count, ip =
          match !pending with
          | Some (pid, period, ip) -> pid, period, ip
          | None -> pid, 1L, None
        in
        pending := None;
        sample ~pid ~count ~ip ~branches:entries);
      loop ()
  in
  loop ();
  match executable with
  | Some executable when Pid_tbl.length processes = 0 ->
    failwith
      (Printf.sprintf
         "the perf data has no mapping of %s; was it recorded running this \
          executable?"
         executable.path)
  | Some _ | None -> ()

let collect ~executable ~perf_data ~f =
  let fields, events =
    match executable with
    | Some _ ->
      "pid,period,ip,brstack", ["--show-mmap-events"; "--show-task-events"]
    | None -> "period,ip,brstack", []
  in
  let args =
    Array.of_list
      (["perf"; "script"; "-i"; perf_data; "--no-demangle"]
      @ events @ ["-F"; fields])
  in
  let ic = Unix.open_process_args_in "perf" args in
  let saw_branches = ref false in
  (try
     iter_channel ?executable ic ~f:(fun sample ->
         if not (List.is_empty sample.branches) then saw_branches := true;
         f sample)
   with exn ->
     ignore (Unix.close_process_in ic : Unix.process_status);
     raise exn);
  (match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
    failwith (Printf.sprintf "'perf script -i %s' failed" perf_data));
  if not !saw_branches
  then
    failwith
      (Printf.sprintf
         "%s has no branch stacks; record with perf record -j any,u" perf_data)
