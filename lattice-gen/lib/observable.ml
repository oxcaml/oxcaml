type mode =
  | Check
  | Update

exception Error of string

let bprintf = Printf.bprintf

type lat_attrs =
  { root_module : string option;
    include_solver : bool
  }

type source_kind =
  | Lat
  | Ocaml

type managed_output =
  { tag_name : string;
    raw : string
  }

type block =
  { kind : source_kind;
    ordinal : int;
    source_raw : string;
    source_body : string;
    gap_before_output : string;
    existing_output : managed_output option;
    lat_attrs : lat_attrs option
  }

type segment =
  | Text of string
  | Block of block

type lat_render =
  { ordinal : int;
    module_name : string;
    attrs : lat_attrs;
    outputs : Generate.outputs
  }

type ocaml_result =
  | Stdout of string
  | Compile_error of string
  | Runtime_error of string
  | Skipped of string

let css_file = "lattice-observable.css"

let js_file = "lattice-observable.js"

let css_contents =
  {|
lat,
ocaml {
  display: block;
  margin: 0.9rem 0;
}

.lattice-observable {
  border: 1px solid #d7dde6;
  border-radius: 10px;
  margin: 0.75rem 0;
  overflow: hidden;
  background: #f8fafc;
  color: #0f172a;
  font: 13px/1.45 Menlo, Monaco, Consolas, monospace;
}

.lattice-observable__header {
  display: flex;
  gap: 0.75rem;
  align-items: center;
  justify-content: space-between;
  padding: 0.6rem 0.85rem;
  border-bottom: 1px solid #d7dde6;
  background: #eef3f8;
  font-family: ui-sans-serif, system-ui, sans-serif;
  font-size: 12px;
}

.lattice-observable__title {
  font-weight: 600;
}

.lattice-observable__meta {
  color: #475569;
}

.lattice-observable__tabs {
  display: flex;
  gap: 0.35rem;
  padding: 0.55rem 0.75rem 0;
  background: #f8fafc;
}

.lattice-observable__tab {
  border: 1px solid #cbd5e1;
  border-bottom: 0;
  border-radius: 8px 8px 0 0;
  padding: 0.35rem 0.7rem;
  background: #e2e8f0;
  color: #334155;
  cursor: pointer;
  font: 12px/1.2 ui-sans-serif, system-ui, sans-serif;
}

.lattice-observable__tab.is-active {
  background: #0f172a;
  color: white;
  border-color: #0f172a;
}

.lattice-observable__panel {
  display: none;
  margin: 0;
  padding: 0.9rem;
  overflow-x: auto;
  background: white;
  border-top: 1px solid #d7dde6;
}

.lattice-observable__panel.is-active {
  display: block;
}

.lattice-observable__panel,
.lattice-source__code {
  white-space: pre;
}

.lattice-source {
  border: 1px solid #d7dde6;
  border-radius: 10px;
  margin: 0.75rem 0;
  overflow: hidden;
  background: #f8fafc;
  color: #0f172a;
}

.lattice-source__header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 0.75rem;
  padding: 0.6rem 0.85rem;
  border-bottom: 1px solid #d7dde6;
  background: #eef3f8;
  font: 12px/1.2 ui-sans-serif, system-ui, sans-serif;
}

.lattice-source__title {
  font-weight: 600;
}

.lattice-source__meta {
  color: #475569;
}

.lattice-source__code {
  margin: 0;
  padding: 0.9rem;
  overflow-x: auto;
  background: white;
  color: #0f172a;
  font: 13px/1.45 Menlo, Monaco, Consolas, monospace;
}

.lattice-source--lat .lattice-source__header {
  background: linear-gradient(90deg, #eef6ff, #f8fafc);
}

.lattice-source--ocaml .lattice-source__header {
  background: linear-gradient(90deg, #f3fdf7, #f8fafc);
}

.tok-keyword {
  color: #7c3aed;
  font-weight: 600;
}

.tok-type {
  color: #0f766e;
}

.tok-ctor {
  color: #b45309;
}

.tok-number {
  color: #1d4ed8;
}

.tok-string {
  color: #047857;
}

.tok-comment {
  color: #64748b;
  font-style: italic;
}

.tok-op {
  color: #be123c;
}

.tok-punct {
  color: #475569;
}

.tok-ident {
  color: #0f172a;
}

.lattice-observable--error {
  border-color: #e11d48;
}

.lattice-observable--error .lattice-observable__header {
  background: #fff1f2;
  border-bottom-color: #fecdd3;
}

.lattice-observable--ok .lattice-observable__header {
  background: #ecfdf5;
  border-bottom-color: #bbf7d0;
}
|}

let js_contents =
  {|
function latticeEscapeHtml(text) {
  return text
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#39;");
}

function latticeSpan(cls, text) {
  return '<span class="' + cls + '">' + latticeEscapeHtml(text) + "</span>";
}

function latticeHighlight(text, language) {
  const ocamlKeywords = new Set([
    "and", "as", "begin", "class", "constraint", "do", "done", "downto",
    "else", "end", "exception", "external", "false", "for", "fun",
    "function", "functor", "if", "in", "include", "inherit", "initializer",
    "lazy", "let", "match", "method", "module", "mutable", "new", "nonrec",
    "object", "of", "open", "or", "private", "rec", "sig", "struct", "then",
    "to", "true", "try", "type", "val", "virtual", "when", "while", "with"
  ]);
  const latticeKeywords = new Set(["op"]);
  const tokenRe =
    /(\(\*[\s\S]*?\*\)|"(?:\\.|[^"\\])*"|\b\d+\b|\b[A-Z][A-Za-z0-9_']*\b|\b[a-z_][A-Za-z0-9_']*\b|->|<=|>=|<>|&&|\|\||[<>=|^:+\-*/]+|[][(){};,])/g;
  let out = "";
  let last = 0;
  text.replace(tokenRe, function (token, _m, offset) {
    out += latticeEscapeHtml(text.slice(last, offset));
    let cls = null;
    if (token.startsWith("(*")) cls = "tok-comment";
    else if (token.startsWith('"')) cls = "tok-string";
    else if (/^\d+$/.test(token)) cls = "tok-number";
    else if (/^[<>=|^:+\-*/]+$/.test(token) || /^(->|<=|>=|<>|&&|\|\|)$/.test(token)) cls = "tok-op";
    else if (/^[\[\](){};,]$/.test(token)) cls = "tok-punct";
    else if (/^[A-Z]/.test(token)) {
      cls = language === "lattice" ? "tok-ident" : "tok-ctor";
    } else if (language === "ocaml" && ocamlKeywords.has(token)) {
      cls = "tok-keyword";
    } else if (language === "lattice" && latticeKeywords.has(token)) {
      cls = "tok-keyword";
    } else {
      cls = language === "ocaml" ? "tok-ident" : "tok-type";
    }
    out += latticeSpan(cls, token);
    last = offset + token.length;
    return token;
  });
  out += latticeEscapeHtml(text.slice(last));
  return out;
}

function latticeEnhanceSource(node, language, title, meta) {
  const text = node.textContent.replace(/^\n+|\n+\s*$/g, "");
  node.classList.add("lattice-source", "lattice-source--" + language);
  node.innerHTML =
    '<div class="lattice-source__header">' +
      '<span class="lattice-source__title">' + latticeEscapeHtml(title) + "</span>" +
      '<span class="lattice-source__meta">' + latticeEscapeHtml(meta) + "</span>" +
    "</div>" +
    '<pre class="lattice-source__code">' + latticeHighlight(text, language) + "</pre>";
}

function latticeEnhancePanels() {
  document.querySelectorAll(".lattice-observable__panel").forEach(function (node) {
    const panel = node.getAttribute("data-lattice-panel");
    const language = panel === "ml" || panel === "mli" || panel === "test" ? "ocaml" : "text";
    if (language === "text") return;
    node.innerHTML = latticeHighlight(node.textContent, language);
  });
}

function latticeEnhanceSources() {
  document.querySelectorAll("lat").forEach(function (node) {
    latticeEnhanceSource(node, "lattice", "Lattice DSL", "source");
  });
  document.querySelectorAll("ocaml").forEach(function (node) {
    latticeEnhanceSource(node, "ocaml", "OCaml snippet", "source");
  });
}

document.addEventListener("DOMContentLoaded", function () {
  latticeEnhanceSources();
  latticeEnhancePanels();
});

document.addEventListener("click", function (event) {
  const button = event.target.closest("[data-lattice-tab]");
  if (!button) return;
  const root = button.closest(".lattice-observable");
  if (!root) return;
  const tab = button.getAttribute("data-lattice-tab");
  root.querySelectorAll("[data-lattice-tab]").forEach(function (node) {
    node.classList.toggle("is-active", node === button);
  });
  root.querySelectorAll("[data-lattice-panel]").forEach(function (node) {
    node.classList.toggle(
      "is-active",
      node.getAttribute("data-lattice-panel") === tab
    );
  });
});
|}

let failf fmt = Printf.ksprintf (fun message -> raise (Error message)) fmt

let read_file path = In_channel.with_open_bin path In_channel.input_all

let write_file path contents =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc contents)

let write_solver_runtime path =
  write_file path Solver_runtime_source.contents

let ensure_dir path =
  if not (Sys.file_exists path) then Unix.mkdir path 0o755

let remove_dir path =
  ignore (Sys.command ("rm -rf " ^ Filename.quote path))

let remove_dir_if_empty path =
  if Sys.file_exists path && Sys.is_directory path && Array.length (Sys.readdir path) = 0
  then Unix.rmdir path

let keep_temp_dirs () =
  match Sys.getenv_opt "LATTICE_GEN_KEEP_TEMP_DIRS" with
  | Some ("1" | "true" | "yes") -> true
  | _ -> false

let prune_stale_temp_dirs base =
  let now = Unix.time () in
  if Sys.file_exists base && Sys.is_directory base
  then
    Sys.readdir base
    |> Array.to_list
    |> List.iter (fun entry ->
         let path = Filename.concat base entry in
         match
           try Some (Sys.is_directory path) with
           | Sys_error _ -> None
         with
         | Some true -> (
           match
             try Some (Unix.stat path) with
             | Unix.Unix_error _ | Sys_error _ -> None
           with
           | Some stats when now -. stats.Unix.st_mtime > 3600.0 -> remove_dir path
           | _ -> ())
         | _ -> ())

let command_output ~cwd command =
  let ic =
    Unix.open_process_in
      (Printf.sprintf "cd %s && %s" (Filename.quote cwd) command)
  in
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file -> List.rev acc
  in
  let lines =
    Fun.protect
      ~finally:(fun () -> ignore (Unix.close_process_in ic))
      (fun () -> loop [])
  in
  String.concat "\n" lines

let rec find_ancestor path predicate =
  if predicate path
  then Some path
  else
    let parent = Filename.dirname path in
    if parent = path then None else find_ancestor parent predicate

let project_root () =
  let exe =
    try Unix.realpath Sys.executable_name with
    | Unix.Unix_error _ -> Sys.executable_name
  in
  let start =
    if Filename.is_relative exe
    then Filename.concat (Sys.getcwd ()) exe
    else exe
  in
  match
    find_ancestor
      (Filename.dirname start)
      (fun dir -> Sys.file_exists (Filename.concat dir "dune-project"))
  with
  | Some root -> root
  | None -> failf "could not find lattice-gen project root from %s" start

let compiler_root () =
  let lines = command_output ~cwd:(project_root ()) "git worktree list --porcelain" in
  let lines = String.split_on_char '\n' lines in
  let roots =
    List.filter_map
      (fun line ->
        if String.length line > 9 && String.sub line 0 9 = "worktree "
        then Some (String.sub line 9 (String.length line - 9))
        else None)
      lines
  in
  let is_configured root =
    Sys.file_exists (Filename.concat root "typing/solver_intf.mli")
    && Sys.file_exists (Filename.concat root "dune.runtime_selection")
    && Sys.file_exists (Filename.concat root "duneconf/dirs-to-ignore.inc")
    && Sys.file_exists (Filename.concat root "duneconf/ox-extra.inc")
  in
  match List.find_opt is_configured roots with
  | Some root -> root
  | None -> failf "could not find configured compiler worktree"

let with_temp_dir prefix f =
  let base = Filename.concat (compiler_root ()) "lattice-gen-observable-build" in
  ensure_dir base;
  prune_stale_temp_dirs base;
  let dir = Filename.temp_file ~temp_dir:base prefix "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  match f dir with
  | result ->
    remove_dir dir;
    remove_dir_if_empty base;
    result
  | exception exn ->
    if keep_temp_dirs ()
    then prerr_endline ("preserving failing temp dir: " ^ dir)
    else (
      remove_dir dir;
      remove_dir_if_empty base);
    raise exn

let run_capture ~cwd command =
  let ic =
    Unix.open_process_in
      (Printf.sprintf
         "cd %s && (%s) 2>&1"
         (Filename.quote cwd)
         command)
  in
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file -> List.rev acc
  in
  let lines = loop [] in
  let status = Unix.close_process_in ic in
  status, String.concat "\n" lines

let escape_html text =
  let buf = Buffer.create (String.length text + 32) in
  String.iter
    (function
      | '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '"' -> Buffer.add_string buf "&quot;"
      | '\'' -> Buffer.add_string buf "&#39;"
      | c -> Buffer.add_char buf c)
    text;
  Buffer.contents buf

let starts_with_at source pos prefix =
  let len = String.length prefix in
  pos + len <= String.length source
  && String.sub source pos len = prefix

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let substring source a b = String.sub source a (b - a)

let skip_spaces source pos =
  let rec loop i =
    if i < String.length source && is_space source.[i] then loop (i + 1) else i
  in
  loop pos

let find_char_from source start ch =
  let rec loop i =
    if i >= String.length source then None
    else if source.[i] = ch then Some i
    else loop (i + 1)
  in
  loop start

let find_string_from source start needle =
  let n_len = String.length needle in
  let s_len = String.length source in
  let rec loop i =
    if i + n_len > s_len
    then None
    else if String.sub source i n_len = needle
    then Some i
    else loop (i + 1)
  in
  loop start

let is_tag_boundary = function
  | None -> true
  | Some ('>' | ' ' | '\t' | '\n' | '\r') -> true
  | _ -> false

let parse_attrs raw =
  let len = String.length raw in
  let rec parse i acc =
    let i = skip_spaces raw i in
    if i >= len
    then List.rev acc
    else (
      let j = ref i in
      while !j < len && raw.[!j] <> '=' && not (is_space raw.[!j]) do
        incr j
      done;
      if !j = i then failf "malformed attributes";
      let key = substring raw i !j in
      let j = skip_spaces raw !j in
      if j >= len || raw.[j] <> '=' then failf "expected = after attribute %S" key;
      let j = skip_spaces raw (j + 1) in
      if j >= len || raw.[j] <> '"' then failf "expected quoted value for %S" key;
      let value_end =
        match find_char_from raw (j + 1) '"' with
        | Some pos -> pos
        | None -> failf "unterminated attribute value for %S" key
      in
      let value = substring raw (j + 1) value_end in
      parse (value_end + 1) ((key, value) :: acc))
  in
  parse 0 []

let parse_open_tag source pos =
  if pos >= String.length source || source.[pos] <> '<' then failf "expected <";
  let close_pos =
    match find_char_from source pos '>' with
    | Some p -> p
    | None -> failf "unterminated tag"
  in
  let raw = substring source pos (close_pos + 1) in
  let inside = substring source (pos + 1) close_pos in
  let inside = String.trim inside in
  let name_end =
    let rec loop i =
      if i >= String.length inside || is_space inside.[i] then i else loop (i + 1)
    in
    loop 0
  in
  let name = substring inside 0 name_end in
  let attrs =
    parse_attrs (substring inside name_end (String.length inside))
  in
  name, attrs, raw, close_pos + 1

let parse_source_kind = function
  | "lat" -> Some Lat
  | "ocaml" -> Some Ocaml
  | _ -> None

let managed_output_tag = function
  | Lat -> "out"
  | Ocaml -> "ocaml-out"

let default_module_name index = Printf.sprintf "Lat_%03d" index

let bool_attr attrs key default =
  match List.assoc_opt key attrs with
  | None -> default
  | Some "true" -> true
  | Some "false" -> false
  | Some value -> failf "attribute %S must be true or false, got %S" key value

let parse_lat_attrs attrs index =
  List.iter
    (fun (key, _) ->
      if
        not
          (List.mem
             key
             [ "root-module"; "include-solver" ])
      then failf "unknown <lat> attribute %S" key)
    attrs;
  let root_module =
    match List.assoc_opt "root-module" attrs with
    | Some name ->
      if not (Name.is_module_name name)
      then failf "invalid root-module %S in <lat> block %d" name index;
      Some name
    | None -> None
  in
  { root_module;
    include_solver = bool_attr attrs "include-solver" true
  }

let parse_ocaml_attrs attrs index =
  match attrs with
  | [] -> ()
  | (key, _) :: _ -> failf "unknown <ocaml> attribute %S in block %d" key index

let scan_document source =
  let len = String.length source in
  let rec next_source_tag pos =
    if pos >= len
    then None
    else
      match find_char_from source pos '<' with
      | None -> None
      | Some i -> (
        match
          parse_source_kind
            (if starts_with_at source i "<lat"
             then "lat"
             else if starts_with_at source i "<ocaml"
             then "ocaml"
             else "")
        with
        | Some kind ->
          let boundary =
            let offset = match kind with Lat -> 4 | Ocaml -> 6 in
            let ch =
              if i + offset < len then Some source.[i + offset] else None
            in
            is_tag_boundary ch
          in
          if boundary then Some (i, kind) else next_source_tag (i + 1)
        | None -> next_source_tag (i + 1))
  in
  let rec loop pos source_count lat_count acc =
    match next_source_tag pos with
    | None ->
      List.rev (Text (substring source pos len) :: acc)
    | Some (tag_pos, kind) ->
      let text = substring source pos tag_pos in
      let name, attrs, _open_raw, body_start = parse_open_tag source tag_pos in
      ignore name;
      let close_name = match kind with Lat -> "</lat>" | Ocaml -> "</ocaml>" in
      let close_start =
        match find_string_from source body_start close_name with
        | Some p -> p
        | None -> failf "missing closing %s tag" close_name
      in
      let close_end = close_start + String.length close_name in
      let source_raw = substring source tag_pos close_end in
      let source_body = substring source body_start close_start in
      let gap_end = skip_spaces source close_end in
      let output_tag = managed_output_tag kind in
      let existing_output, next_pos, gap_before_output =
        if starts_with_at source gap_end ("<" ^ output_tag)
           && is_tag_boundary
                (if gap_end + String.length output_tag + 1 < len
                 then Some source.[gap_end + String.length output_tag + 1]
                 else None)
        then
          let _, _, _, out_body_start = parse_open_tag source gap_end in
          let close_tag = "</" ^ output_tag ^ ">" in
          let out_close_start =
            match find_string_from source out_body_start close_tag with
            | Some p -> p
            | None -> failf "missing closing %s tag" close_tag
          in
          let out_close_end = out_close_start + String.length close_tag in
          ( Some
              { tag_name = output_tag;
                raw = substring source gap_end out_close_end
              },
            out_close_end,
            substring source close_end gap_end )
        else None, close_end, ""
      in
      let ordinal = source_count + 1 in
      let lat_attrs =
        match kind with
        | Lat ->
          let lat_index = lat_count + 1 in
          Some (parse_lat_attrs attrs lat_index)
        | Ocaml ->
          parse_ocaml_attrs attrs ordinal;
          None
      in
      let block =
        { kind;
          ordinal;
          source_raw;
          source_body;
          gap_before_output;
          existing_output;
          lat_attrs
        }
      in
      let lat_count =
        match kind with
        | Lat -> lat_count + 1
        | Ocaml -> lat_count
      in
      loop next_pos ordinal lat_count (Block block :: Text text :: acc)
  in
  match loop 0 0 0 [] with
  | Text "" :: rest -> rest
  | xs -> xs

let rel_path ~from_dir ~to_path =
  let split path =
    String.split_on_char '/' path |> List.filter (fun s -> s <> "")
  in
  let from_parts = split from_dir in
  let to_parts = split to_path in
  let rec drop_common a b =
    match a, b with
    | x :: xs, y :: ys when x = y -> drop_common xs ys
    | _ -> a, b
  in
  let from_tail, to_tail = drop_common from_parts to_parts in
  let up = List.map (fun _ -> "..") from_tail in
  match up @ to_tail with
  | [] -> "."
  | parts -> String.concat "/" parts

let asset_tags ~file_path ~asset_root =
  let file_dir = Filename.dirname file_path in
  let css_href = rel_path ~from_dir:file_dir ~to_path:(Filename.concat asset_root css_file) in
  let js_src = rel_path ~from_dir:file_dir ~to_path:(Filename.concat asset_root js_file) in
  Printf.sprintf
    "<link rel=\"stylesheet\" href=\"%s\" data-lattice-observable=\"css\">\n<script src=\"%s\" data-lattice-observable=\"js\"></script>\n"
    css_href
    js_src

let ensure_assets_in_html source ~file_path ~asset_root =
  let tags = asset_tags ~file_path ~asset_root in
  let has_css =
    Option.is_some
      (find_string_from source 0 "data-lattice-observable=\"css\"")
  in
  let has_js =
    Option.is_some
      (find_string_from source 0 "data-lattice-observable=\"js\"")
  in
  if has_css && has_js
  then source
  else
    match find_string_from source 0 "</head>" with
    | Some pos -> substring source 0 pos ^ tags ^ substring source pos (String.length source)
    | None -> tags ^ source

let write_if_changed path contents =
  if Sys.file_exists path && read_file path = contents
  then ()
  else write_file path contents

let render_lat_output ~module_name (outputs : Generate.outputs) =
  let ml_name = module_name ^ ".ml" in
  let mli_name = module_name ^ ".mli" in
  let test_name = module_name ^ "_test.ml" in
  let panel key title body active =
    Printf.sprintf
      "<button class=\"lattice-observable__tab%s\" data-lattice-tab=\"%s\">%s</button>"
      (if active then " is-active" else "")
      key
      title,
    Printf.sprintf
      "<pre class=\"lattice-observable__panel%s\" data-lattice-panel=\"%s\">%s</pre>"
      (if active then " is-active" else "")
      key
      (escape_html body)
  in
  let ml_tab, ml_panel = panel "ml" ml_name outputs.ml true in
  let mli_tab, mli_panel = panel "mli" mli_name outputs.mli false in
  let test_tab, test_panel = panel "test" test_name outputs.test_ml false in
  String.concat
    ""
    [ "<out>\n";
      "<div class=\"lattice-observable lat-observable\" data-default-tab=\"ml\">\n";
      "<div class=\"lattice-observable__header\"><span class=\"lattice-observable__title\">";
      escape_html module_name;
      "</span><span class=\"lattice-observable__meta\">generated lattice library</span></div>\n";
      "<div class=\"lattice-observable__tabs\">";
      ml_tab;
      mli_tab;
      test_tab;
      "</div>\n";
      ml_panel;
      mli_panel;
      test_panel;
      "\n</div>\n";
      "</out>"
    ]

let render_ocaml_output = function
  | Stdout text ->
    Printf.sprintf
      "<ocaml-out>\n<div class=\"lattice-observable lattice-observable--ok\">\
       \n<div class=\"lattice-observable__header\"><span class=\"lattice-observable__title\">OCaml stdout</span></div>\
       \n<pre class=\"lattice-observable__panel is-active\">%s</pre>\n</div>\n</ocaml-out>"
      (escape_html text)
  | Compile_error text ->
    Printf.sprintf
      "<ocaml-out>\n<div class=\"lattice-observable lattice-observable--error\">\
       \n<div class=\"lattice-observable__header\"><span class=\"lattice-observable__title\">OCaml compile error</span></div>\
       \n<pre class=\"lattice-observable__panel is-active\">%s</pre>\n</div>\n</ocaml-out>"
      (escape_html text)
  | Runtime_error text ->
    Printf.sprintf
      "<ocaml-out>\n<div class=\"lattice-observable lattice-observable--error\">\
       \n<div class=\"lattice-observable__header\"><span class=\"lattice-observable__title\">OCaml runtime error</span></div>\
       \n<pre class=\"lattice-observable__panel is-active\">%s</pre>\n</div>\n</ocaml-out>"
      (escape_html text)
  | Skipped text ->
    Printf.sprintf
      "<ocaml-out>\n<div class=\"lattice-observable\">\
       \n<div class=\"lattice-observable__header\"><span class=\"lattice-observable__title\">OCaml skipped</span></div>\
       \n<pre class=\"lattice-observable__panel is-active\">%s</pre>\n</div>\n</ocaml-out>"
      (escape_html text)

let lat_block_indices segments =
  List.filter_map
    (function
      | Block { kind = Lat; ordinal; _ } -> Some ordinal
      | _ -> None)
    segments

let render_lat_blocks segments file_path =
  let seen = Hashtbl.create 16 in
  let lat_count = ref 0 in
  List.filter_map
    (function
      | Text _ -> None
      | Block block -> (
        match block.kind with
        | Ocaml -> None
        | Lat ->
          incr lat_count;
          let attrs =
            match block.lat_attrs with
            | Some attrs -> attrs
            | None -> failwith "missing lat attrs"
          in
          let module_name =
            match attrs.root_module with
            | Some name -> name
            | None -> default_module_name !lat_count
          in
          if Hashtbl.mem seen module_name
          then failf "duplicate root module %S in %s" module_name file_path;
          Hashtbl.add seen module_name ();
          let outputs =
            Generate.render_string
              ~config:
                { Render_config.include_solver = attrs.include_solver }
              ~root_module:module_name
              ~input_name:(Printf.sprintf "%s:lat-%03d" file_path block.ordinal)
              block.source_body
          in
          Some
            { ordinal = block.ordinal;
              module_name;
              attrs;
              outputs
            }))
    segments

let generate_page_main snippets file_path =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "[@@@warning \"-32\"]\n\n";
  List.iteri
    (fun index snippet ->
      let block_no = index + 1 in
      let start_marker = Printf.sprintf "__LATTICE_OCAML_BLOCK_%03d_START__" block_no in
      let end_marker = Printf.sprintf "__LATTICE_OCAML_BLOCK_%03d_END__" block_no in
      bprintf buf "let () = print_endline %S\n" start_marker;
      bprintf buf "# 1 %S\n" (Printf.sprintf "%s:ocaml-%03d" file_path block_no);
      Buffer.add_string buf snippet;
      if snippet = "" || snippet.[String.length snippet - 1] <> '\n'
      then Buffer.add_char buf '\n';
      bprintf buf "# 1 %S\n" "page_main.ml";
      bprintf buf "let () = print_endline %S\n\n" end_marker)
    snippets;
  Buffer.contents buf

let block_marker index kind =
  Printf.sprintf "__LATTICE_OCAML_BLOCK_%03d_%s__" index kind

let extract_block_output stdout index =
  let start_marker = block_marker index "START" in
  let end_marker = block_marker index "END" in
  match find_string_from stdout 0 start_marker with
  | None -> ""
  | Some start ->
    let after_start = start + String.length start_marker in
    let after_start =
      if after_start < String.length stdout && stdout.[after_start] = '\n'
      then after_start + 1
      else after_start
    in
    (match find_string_from stdout after_start end_marker with
     | Some stop -> substring stdout after_start stop |> String.trim
     | None -> substring stdout after_start (String.length stdout) |> String.trim)

let write_workspace_dune path modules =
  let content =
    Printf.sprintf
      "(executable\n (name page_main)\n (modules %s)\n (libraries ocamlcommon))\n"
      (String.concat " " modules)
  in
  write_file path content

let evaluate_ocaml_blocks ~file_path ~lat_renders segments =
  let lat_by_ordinal = Hashtbl.create 16 in
  List.iter (fun lat -> Hashtbl.add lat_by_ordinal lat.ordinal lat) lat_renders;
  let rec loop lats_so_far snippet_texts ocaml_index acc = function
    | [] -> List.rev acc
    | Text _ :: rest -> loop lats_so_far snippet_texts ocaml_index acc rest
    | Block block :: rest -> (
      match block.kind with
      | Lat ->
        let lat =
          try Hashtbl.find lat_by_ordinal block.ordinal
          with Not_found -> failwith "missing lat render"
        in
        loop (lats_so_far @ [ lat ]) snippet_texts ocaml_index acc rest
      | Ocaml ->
        let ocaml_index = ocaml_index + 1 in
        let snippet_texts = snippet_texts @ [ block.source_body ] in
        let result =
          with_temp_dir (Printf.sprintf "observable-%03d-" ocaml_index) (fun dir ->
            List.iter
              (fun lat ->
                let ml_path = Filename.concat dir (lat.module_name ^ ".ml") in
                let mli_path = Filename.concat dir (lat.module_name ^ ".mli") in
                write_file ml_path lat.outputs.ml;
                write_file mli_path lat.outputs.mli)
              lats_so_far;
            if List.exists (fun lat -> lat.attrs.include_solver) lats_so_far
            then write_solver_runtime (Filename.concat dir "solver_runtime.ml");
            write_file (Filename.concat dir "page_main.ml")
              (generate_page_main snippet_texts file_path);
            write_workspace_dune
              (Filename.concat dir "dune")
              ((if List.exists (fun lat -> lat.attrs.include_solver) lats_so_far
                then [ "page_main"; "solver_runtime" ]
                else [ "page_main" ])
               @ List.map (fun lat -> lat.module_name) lats_so_far);
            let root = compiler_root () in
            let rel_dir =
              Filename.concat
                "lattice-gen-observable-build"
                (Filename.basename dir)
            in
            match
              run_capture
                ~cwd:root
                (Printf.sprintf
                   "RUNTIME_DIR=runtime4 dune build --display=short %s/page_main.exe"
                   rel_dir)
            with
            | Unix.WEXITED 0, _ -> (
              match
                run_capture
                  ~cwd:root
                  (Printf.sprintf
                     "RUNTIME_DIR=runtime4 dune exec --display=short ./%s/page_main.exe"
                     rel_dir)
              with
              | Unix.WEXITED 0, stdout -> Stdout (extract_block_output stdout ocaml_index)
              | _, output -> Runtime_error output)
            | _, output -> Compile_error output)
        in
        let acc = (block.ordinal, result) :: acc in
        (match result with
         | Stdout _ ->
           loop lats_so_far snippet_texts ocaml_index acc rest
         | Compile_error _ | Runtime_error _ ->
           let rec mark_skipped acc = function
             | [] -> List.rev_append acc []
             | Text _ :: xs -> mark_skipped acc xs
             | Block block :: xs -> (
               match block.kind with
               | Lat -> mark_skipped acc xs
               | Ocaml ->
                 mark_skipped
                   ((block.ordinal, Skipped (Printf.sprintf "skipped because ocaml block %d failed" ocaml_index)) :: acc)
                   xs)
           in
           List.rev_append acc (mark_skipped [] rest)
         | Skipped _ -> failwith "unexpected skipped"))

  in
  loop [] [] 0 [] segments

let collect_blocks_by_kind segments kind =
  List.filter_map
    (function
      | Block block when block.kind = kind -> Some block
      | _ -> None)
    segments

let rewrite_document ~file_path ~asset_root source =
  let segments = scan_document source in
  let lat_renders = render_lat_blocks segments file_path in
  let lat_map = Hashtbl.create 16 in
  List.iter (fun lat -> Hashtbl.add lat_map lat.ordinal lat) lat_renders;
  let ocaml_results = evaluate_ocaml_blocks ~file_path ~lat_renders segments in
  let ocaml_map = Hashtbl.create 16 in
  List.iter (fun (ordinal, result) -> Hashtbl.add ocaml_map ordinal result) ocaml_results;
  let mismatches = ref [] in
  let rendered =
    List.map
      (function
        | Text text -> text
        | Block block -> (
          let expected_output =
            match block.kind with
            | Lat ->
              let lat = Hashtbl.find lat_map block.ordinal in
              render_lat_output ~module_name:lat.module_name lat.outputs
            | Ocaml ->
              let result = Hashtbl.find ocaml_map block.ordinal in
              render_ocaml_output result
          in
          let actual_output =
            match block.existing_output with
            | Some output -> Some output.raw
            | None -> None
          in
          if actual_output <> Some expected_output
          then
            mismatches :=
              (match block.kind with
               | Lat -> Printf.sprintf "lat block %d" block.ordinal
               | Ocaml -> Printf.sprintf "ocaml block %d" block.ordinal)
              :: !mismatches;
          block.source_raw
          ^ block.gap_before_output
          ^ expected_output))
      segments
    |> String.concat ""
  in
  let with_assets = ensure_assets_in_html rendered ~file_path ~asset_root in
  let mismatches =
    if with_assets <> rendered then "assets" :: !mismatches else !mismatches
  in
  with_assets, List.rev mismatches

let rec collect_html_files path =
  if Sys.is_directory path
  then
    Sys.readdir path
    |> Array.to_list
    |> List.sort String.compare
    |> List.concat_map (fun entry ->
         collect_html_files (Filename.concat path entry))
  else if Filename.check_suffix path ".html"
  then [ path ]
  else []

let normalize_root path =
  if Sys.is_directory path then path else Filename.dirname path

let process_root ~mode root files =
  let assets_written = ref false in
  List.iter
    (fun file_path ->
      let source = read_file file_path in
      let rewritten, mismatches = rewrite_document ~file_path ~asset_root:root source in
      if mismatches <> []
      then (
        match mode with
        | Check ->
          failf
            "%s is stale: %s"
            file_path
            (String.concat ", " mismatches)
        | Update ->
          write_file file_path rewritten);
      if mode = Update && not !assets_written
      then (
        write_if_changed (Filename.concat root css_file) css_contents;
        write_if_changed (Filename.concat root js_file) js_contents;
        assets_written := true))
    files;
  if mode = Check
  then (
    let css_path = Filename.concat root css_file in
    let js_path = Filename.concat root js_file in
    if Sys.file_exists css_path
    then (
      if read_file css_path <> css_contents
      then failf "%s is stale" css_path)
    else failf "missing %s" css_path;
    if Sys.file_exists js_path
    then (
      if read_file js_path <> js_contents
      then failf "%s is stale" js_path)
    else failf "missing %s" js_path)

let process_paths ~mode paths =
  let groups = Hashtbl.create 8 in
  List.iter
    (fun path ->
      let root = normalize_root path in
      let files = collect_html_files path in
      let existing =
        match Hashtbl.find_opt groups root with
        | Some files -> files
        | None -> []
      in
      Hashtbl.replace groups root (existing @ files))
    paths;
  if Hashtbl.length groups = 0 then failf "no HTML files found";
  Hashtbl.iter
    (fun root files ->
      let files = List.sort_uniq String.compare files in
      process_root ~mode root files)
    groups
