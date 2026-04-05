type mode =
  | Check
  | Update

exception Error of string

type lat_attrs =
  { root_module : string option }

let failf fmt = Printf.ksprintf (fun msg -> raise (Error msg)) fmt

let read_file path = In_channel.with_open_bin path In_channel.input_all

let write_file path contents =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc contents)

let html_escape text =
  let buf = Buffer.create (String.length text) in
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

let contains_substring haystack needle =
  let h_len = String.length haystack in
  let n_len = String.length needle in
  let rec loop i =
    if i + n_len > h_len
    then false
    else if String.sub haystack i n_len = needle
    then true
    else loop (i + 1)
  in
  loop 0

let find_substring haystack needle =
  let h_len = String.length haystack in
  let n_len = String.length needle in
  let rec loop i =
    if i + n_len > h_len
    then None
    else if String.sub haystack i n_len = needle
    then Some i
    else loop (i + 1)
  in
  loop 0

let marker_start index =
  Printf.sprintf "<!-- lattice-gen-output:start %d -->" index

let marker_end index = Printf.sprintf "<!-- lattice-gen-output:end %d -->" index

let scaffold_start = "<!-- lattice-gen-scaffold:start -->"

let scaffold_end = "<!-- lattice-gen-scaffold:end -->"

let render_scaffold () =
  scaffold_start
  ^ "\n"
  ^ {|
<style>
  lat {
    --tok-comment: #64748b;
    --tok-string: #0f766e;
    --tok-number: #1d4ed8;
    --tok-keyword: #7c3aed;
    --tok-op: #b45309;
    --tok-punct: #475569;
    --tok-ctor: #be123c;
    --tok-type: #0369a1;
    --tok-ident: #0f172a;
    display: block;
    margin: 1.5rem 0;
    padding: 1rem 1.1rem;
    white-space: pre-wrap;
    overflow-x: auto;
    border: 1px solid #dbe4f0;
    border-radius: 14px;
    background:
      linear-gradient(180deg, rgba(248, 250, 252, 0.98), rgba(241, 245, 249, 0.98));
    color: #0f172a;
    font: 13px/1.6 Menlo, Monaco, Consolas, monospace;
    box-shadow: inset 0 1px 0 rgba(255, 255, 255, 0.7);
    tab-size: 2;
  }

  .lattice-generated {
    margin: 1.5rem 0;
    border: 1px solid #cbd5e1;
    border-radius: 14px;
    background: #ffffff;
    box-shadow: 0 12px 30px rgba(15, 23, 42, 0.08);
    overflow: hidden;
  }

  .lattice-generated details {
    margin: 0;
  }

  .lattice-generated summary {
    cursor: pointer;
    list-style: none;
    padding: 0.9rem 1rem;
    font: 600 14px/1.2 ui-sans-serif, system-ui, sans-serif;
    color: #0f172a;
    background: linear-gradient(180deg, #f8fafc, #eef2ff);
    border-bottom: 1px solid #dbe4f0;
  }

  .lattice-generated summary::-webkit-details-marker {
    display: none;
  }

  .lattice-generated-body {
    padding: 0.9rem 1rem 1rem;
  }

  .lattice-generated-tabs {
    display: flex;
    gap: 0.5rem;
    margin: 0 0 0.75rem;
    flex-wrap: wrap;
  }

  .lattice-generated-tab {
    border: 1px solid #cbd5e1;
    border-radius: 999px;
    padding: 0.35rem 0.75rem;
    background: #ffffff;
    color: #334155;
    font: 600 12px/1 ui-sans-serif, system-ui, sans-serif;
  }

  .lattice-generated-tab.is-active {
    background: #0f172a;
    border-color: #0f172a;
    color: #ffffff;
  }

  .lattice-generated-panel {
    display: none;
  }

  .lattice-generated-panel.is-active {
    display: block;
  }

  .lattice-generated pre {
    --tok-comment: #94a3b8;
    --tok-string: #86efac;
    --tok-number: #93c5fd;
    --tok-keyword: #c4b5fd;
    --tok-op: #fbbf24;
    --tok-punct: #cbd5e1;
    --tok-ctor: #fda4af;
    --tok-type: #67e8f9;
    --tok-ident: #e2e8f0;
    margin: 0;
    padding: 1rem;
    overflow-x: auto;
    border-radius: 12px;
    background: #0f172a;
    color: #e2e8f0;
    font: 12px/1.5 Menlo, Monaco, Consolas, monospace;
  }

  .tok-comment { color: var(--tok-comment); }
  .tok-string { color: var(--tok-string); }
  .tok-number { color: var(--tok-number); }
  .tok-keyword { color: var(--tok-keyword); font-weight: 600; }
  .tok-op { color: var(--tok-op); }
  .tok-punct { color: var(--tok-punct); }
  .tok-ctor { color: var(--tok-ctor); }
  .tok-type { color: var(--tok-type); }
  .tok-ident { color: var(--tok-ident); }
</style>
<script>
  (() => {
    const escapeHtml = text =>
      text
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;')
        .replace(/'/g, '&#39;');

    const span = (cls, text) =>
      '<span class="' + cls + '">' + escapeHtml(text) + '</span>';

    const ocamlKeywords = new Set([
      'and', 'as', 'begin', 'class', 'constraint', 'do', 'done', 'downto',
      'else', 'end', 'exception', 'external', 'false', 'for', 'fun',
      'function', 'functor', 'if', 'in', 'include', 'inherit', 'initializer',
      'lazy', 'let', 'match', 'method', 'module', 'mutable', 'new', 'nonrec',
      'object', 'of', 'open', 'or', 'private', 'rec', 'sig', 'struct', 'then',
      'to', 'true', 'try', 'type', 'val', 'virtual', 'when', 'while', 'with'
    ]);

    const latticeKeywords = new Set(['id', 'max', 'min', 'op', 'via']);

    const tokenRe =
      /(\(\*[\s\S]*?\*\)|"(?:\\.|[^"\\])*"|\b\d+\b|\b[A-Z][A-Za-z0-9_']*\b|\b[a-z_][A-Za-z0-9_'-]*\b|->|<=|>=|<>|&&|\|\||[<>=|^:+\-*/]+|[][(){};,])/g;

    const highlight = (text, language) => {
      let out = '';
      let last = 0;
      text.replace(tokenRe, (token, _m, offset) => {
        out += escapeHtml(text.slice(last, offset));
        let cls;
        if (token.startsWith('(*')) cls = 'tok-comment';
        else if (token.startsWith('"')) cls = 'tok-string';
        else if (/^\d+$/.test(token)) cls = 'tok-number';
        else if (
          /^[<>=|^:+\-*/]+$/.test(token) ||
          /^(->|<=|>=|<>|&&|\|\|)$/.test(token)
        ) cls = 'tok-op';
        else if (/^[\[\](){};,]$/.test(token)) cls = 'tok-punct';
        else if (/^[A-Z]/.test(token)) cls = language === 'lattice' ? 'tok-ident' : 'tok-ctor';
        else if (language === 'ocaml' && ocamlKeywords.has(token)) cls = 'tok-keyword';
        else if (language === 'lattice' && latticeKeywords.has(token)) cls = 'tok-keyword';
        else cls = language === 'ocaml' ? 'tok-ident' : 'tok-type';
        out += span(cls, token);
        last = offset + token.length;
        return token;
      });
      out += escapeHtml(text.slice(last));
      return out;
    };

    const highlightLatBlocks = () => {
      for (const block of document.querySelectorAll('lat')) {
        const text = block.textContent.replace(/^\n+|\n+\s*$/g, '');
        block.innerHTML = highlight(text, 'lattice');
      }
    };

    const highlightGeneratedBlocks = () => {
      for (const pre of document.querySelectorAll('.lattice-generated pre[data-language]')) {
        pre.innerHTML = highlight(pre.textContent, pre.dataset.language);
      }
    };

    const init = () => {
      highlightLatBlocks();
      highlightGeneratedBlocks();
      const blocks = document.querySelectorAll('.lattice-generated');
      for (const block of blocks) {
        const tabs = block.querySelectorAll('.lattice-generated-tab');
        const panels = block.querySelectorAll('.lattice-generated-panel');
        const activate = kind => {
          for (const tab of tabs) {
            tab.classList.toggle('is-active', tab.dataset.panel === kind);
          }
          for (const panel of panels) {
            panel.classList.toggle('is-active', panel.dataset.panel === kind);
          }
        };
        for (const tab of tabs) {
          tab.addEventListener('click', () => activate(tab.dataset.panel));
        }
        activate('ml');
      }
    };
    if (document.readyState === 'loading') {
      document.addEventListener('DOMContentLoaded', init, { once: true });
    } else {
      init();
    }
  })();
</script>
|}
  ^ "\n"
  ^ scaffold_end
  ^ "\n"

let strip_generated_blocks source =
  let rec loop acc pos =
    match String.index_from_opt source pos '<' with
    | None ->
      Buffer.add_substring acc source pos (String.length source - pos);
      Buffer.contents acc
    | Some i ->
      let start_marker = "<!-- lattice-gen-output:start " in
      if
        i + String.length start_marker <= String.length source
        && String.sub source i (String.length start_marker) = start_marker
      then (
        let copy_end =
          if i > pos && source.[i - 1] = '\n' then i - 1 else i
        in
        Buffer.add_substring acc source pos (copy_end - pos);
        match String.index_from_opt source i '\n' with
        | None -> Buffer.contents acc
        | Some after_start ->
          let rec find_end j =
            match String.index_from_opt source j '<' with
            | None -> failf "unterminated generated observable block"
            | Some k ->
              let end_prefix = "<!-- lattice-gen-output:end " in
              if
                k + String.length end_prefix <= String.length source
                && String.sub source k (String.length end_prefix) = end_prefix
              then (
                match String.index_from_opt source k '>' with
                | None -> failf "unterminated generated observable block"
                | Some gt ->
                  let next =
                    if gt + 1 < String.length source && source.[gt + 1] = '\n'
                    then gt + 2
                    else gt + 1
                  in
                  loop acc next)
              else find_end (k + 1)
          in
          find_end (after_start + 1))
      else if
        i + String.length scaffold_start <= String.length source
        && String.sub source i (String.length scaffold_start) = scaffold_start
      then (
        let copy_end =
          if i > pos && source.[i - 1] = '\n' then i - 1 else i
        in
        Buffer.add_substring acc source pos (copy_end - pos);
        let after_start =
          match String.index_from_opt source i '\n' with
          | None -> failf "unterminated generated observable scaffold"
          | Some after_start -> after_start
        in
        let rec find_scaffold_end j =
          match String.index_from_opt source j '<' with
          | None -> failf "unterminated generated observable scaffold"
          | Some k ->
            if
              k + String.length scaffold_end <= String.length source
              && String.sub source k (String.length scaffold_end) = scaffold_end
            then (
              match String.index_from_opt source k '>' with
              | None -> failf "unterminated generated observable scaffold"
              | Some gt ->
                let next =
                  if gt + 1 < String.length source && source.[gt + 1] = '\n'
                  then gt + 2
                  else gt + 1
                in
                loop acc next)
            else find_scaffold_end (k + 1)
        in
        find_scaffold_end (after_start + 1))
      else (
        Buffer.add_substring acc source pos (i - pos + 1);
        loop acc (i + 1))
  in
  loop (Buffer.create (String.length source)) 0

let parse_attrs raw =
  let len = String.length raw in
  let is_space = function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false
  in
  let rec skip i =
    if i < len && is_space raw.[i] then skip (i + 1) else i
  in
  let rec ident j =
    if j < len then
      match raw.[j] with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' -> ident (j + 1)
      | _ -> j
    else j
  in
  let rec parse i root_module =
    let i = skip i in
    if i >= len
    then { root_module }
    else
      let j = ident i in
      if j = i then failf "malformed <lat> attributes";
      let key = String.sub raw i (j - i) in
      let j = skip j in
      if j >= len || raw.[j] <> '=' then failf "expected '=' after %s" key;
      let j = skip (j + 1) in
      if j >= len || raw.[j] <> '"' then failf "expected quoted value for %s" key;
      let k =
        match String.index_from_opt raw (j + 1) '"' with
        | Some k -> k
        | None -> failf "unterminated attribute %s" key
      in
      let value = String.sub raw (j + 1) (k - j - 1) in
      let root_module =
        match key with
        | "root-module" -> Some value
        | _ -> failf "unknown <lat> attribute %S" key
      in
      parse (k + 1) root_module
  in
  parse 0 None

let render_block index attrs body =
  let outputs =
    Generate.render_string ?root_module:attrs.root_module ~input_name:"observable" body
  in
  Printf.sprintf
    "%s\n\
     <div class=\"lattice-generated\">\n\
     <details open>\n\
     <summary>Generated %d</summary>\n\
     <div class=\"lattice-generated-body\">\n\
     <div class=\"lattice-generated-tabs\">\n\
     <button type=\"button\" class=\"lattice-generated-tab\" data-panel=\"ml\">.ml</button>\n\
     <button type=\"button\" class=\"lattice-generated-tab\" data-panel=\"mli\">.mli</button>\n\
     </div>\n\
     <div class=\"lattice-generated-panel\" data-panel=\"ml\">\n\
     <pre data-language=\"ocaml\">%s</pre>\n\
     </div>\n\
     <div class=\"lattice-generated-panel\" data-panel=\"mli\">\n\
     <pre data-language=\"ocaml\">%s</pre>\n\
     </div>\n\
     </div>\n\
     </details>\n\
     </div>\n\
     %s\n"
    (marker_start index)
    index
    (html_escape outputs.ml)
    (html_escape outputs.mli)
    (marker_end index)

let inject_scaffold source =
  if contains_substring source scaffold_start
  then source
  else
    let scaffold = render_scaffold () in
    match find_substring source "<body>" with
    | Some i ->
      let split = i + String.length "<body>" in
      String.sub source 0 split ^ "\n" ^ scaffold ^ String.sub source split
          (String.length source - split)
    | None ->
      (match find_substring source "<lat" with
       | Some i ->
         String.sub source 0 i ^ scaffold ^ String.sub source i
             (String.length source - i)
       | None -> source ^ "\n" ^ scaffold)

let update_source source =
  if contains_substring source "<ocaml>"
  then failf "<ocaml> blocks are not supported in const-only observable mode";
  if contains_substring source "<ocaml "
  then failf "<ocaml> blocks are not supported in const-only observable mode";
  let source = source |> strip_generated_blocks |> inject_scaffold in
  let len = String.length source in
  let buf = Buffer.create (len + 4096) in
  let rec loop pos index =
    match String.index_from_opt source pos '<' with
    | None -> Buffer.add_substring buf source pos (len - pos)
    | Some i ->
      if i + 4 <= len && String.sub source i 4 = "<lat"
      then (
        Buffer.add_substring buf source pos (i - pos);
        let tag_end =
          match String.index_from_opt source i '>' with
          | Some j -> j
          | None -> failf "unterminated <lat> tag"
        in
        let attrs_raw = String.sub source (i + 4) (tag_end - i - 4) in
        let close = "</lat>" in
        let body_end =
          match String.index_from_opt source (tag_end + 1) '<' with
          | None -> failf "unterminated <lat> block"
          | Some _ ->
            let rec find j =
              match String.index_from_opt source j '<' with
              | None -> failf "unterminated <lat> block"
              | Some k ->
                if k + String.length close <= len
                   && String.sub source k (String.length close) = close
                then k
                else find (k + 1)
            in
            find (tag_end + 1)
        in
        let close_end = body_end + String.length close in
        let body = String.sub source (tag_end + 1) (body_end - tag_end - 1) in
        let attrs = parse_attrs attrs_raw in
        let rec skip_newlines j =
          if j < len && (source.[j] = '\n' || source.[j] = '\r')
          then skip_newlines (j + 1)
          else j
        in
        let next_pos = skip_newlines close_end in
        Buffer.add_substring buf source i (close_end - i);
        Buffer.add_char buf '\n';
        Buffer.add_string buf (render_block index attrs body);
        loop next_pos (index + 1))
      else (
        Buffer.add_substring buf source pos (i - pos + 1);
        loop (i + 1) index)
  in
  loop 0 1;
  Buffer.contents buf

let process_file ~mode path =
  let source = read_file path in
  let updated = update_source source in
  match mode with
  | Update -> if updated <> source then write_file path updated
  | Check ->
    if updated <> source then failf "observable snapshot is stale: %s" path

let rec process_path ~mode path =
  if Sys.is_directory path
  then
    Sys.readdir path
    |> Array.to_list
    |> List.sort String.compare
    |> List.iter (fun entry -> process_path ~mode (Filename.concat path entry))
  else if Filename.check_suffix path ".html"
  then process_file ~mode path
  else ()

let process_paths ~mode paths = List.iter (process_path ~mode) paths
