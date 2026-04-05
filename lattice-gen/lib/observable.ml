type mode =
  | Check
  | Update

exception Error of string

type lat_attrs =
  { root_module : string option }

type lat_block =
  { index : int;
    attrs : lat_attrs;
    body : string
  }

let failf fmt = Printf.ksprintf (fun msg -> raise (Error msg)) fmt

let read_file path = In_channel.with_open_bin path In_channel.input_all

let write_file path contents =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc contents)

let ensure_dir_exists path =
  if Sys.file_exists path
  then (
    if not (Sys.is_directory path)
    then failwith (Printf.sprintf "%s exists and is not a directory" path))
  else Unix.mkdir path 0o755

let remove_dir path =
  ignore (Sys.command ("rm -rf " ^ Filename.quote path))

let with_temp_dir prefix f =
  let base = Filename.concat (Filename.get_temp_dir_name ()) "lattice-gen-observable" in
  ensure_dir_exists base;
  let dir = Filename.temp_file ~temp_dir:base prefix "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  match f dir with
  | result ->
    remove_dir dir;
    result
  | exception exn ->
    remove_dir dir;
    raise exn

let run_command ~cwd command =
  let log_path = Filename.concat cwd "command.log" in
  let full_command =
    Printf.sprintf
      "cd %s && (%s) > %s 2>&1"
      (Filename.quote cwd)
      command
      (Filename.quote log_path)
  in
  match Sys.command full_command with
  | 0 -> ()
  | n ->
    let output =
      try read_file log_path with
      | Sys_error _ -> ""
    in
    failf "command failed (%d) in %s:\n$ %s\n%s" n cwd command output

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

type semantics =
  { values : int array;
    bottom : int;
    top : int;
    leq : int -> int -> bool;
    join : int -> int -> int;
    meet : int -> int -> int;
    sub : int -> int -> int;
    imply : int -> int -> int
  }

let base_finite (base : Model.base) ~opposite =
  if opposite then Model.lattice_reverse base.logical else base.logical

let finite_sub_table (finite : Model.finite_lattice) =
  let n = Array.length finite.element_names in
  Array.init n (fun x ->
      Array.init n (fun y ->
          match
            Model.least_over finite (fun z -> finite.leq.(x).(finite.join.(y).(z)))
          with
          | Some z -> z
          | None ->
            invalid_arg
              (Printf.sprintf
                 "no subtraction result for finite lattice element indices %d and %d"
                 x
                 y)))

let finite_imply_table (finite : Model.finite_lattice) =
  let n = Array.length finite.element_names in
  Array.init n (fun x ->
      Array.init n (fun y ->
          match
            Model.greatest_under finite (fun z -> finite.leq.(finite.meet.(z).(x)).(y))
          with
          | Some z -> z
          | None ->
            invalid_arg
              (Printf.sprintf
                 "no implication result for finite lattice element indices %d and %d"
                 x
                 y)))

let make_semantics_resolver model =
  let cache = Hashtbl.create 32 in
  let rec resolve (expr : Model.lattice_expr) =
    match Hashtbl.find_opt cache expr with
    | Some semantics -> semantics
    | None ->
      let semantics =
        match Model.String_map.find expr.name model.Model.lattices with
        | Model.Base base ->
          let finite = base_finite base ~opposite:expr.opposite in
          let indices = Model.value_index finite in
          let sub = finite_sub_table finite in
          let imply = finite_imply_table finite in
          let index_of value = Model.find_value_index finite indices value in
          { values = Array.copy finite.element_values;
            bottom = finite.element_values.(finite.bottom);
            top = finite.element_values.(finite.top);
            leq = (fun x y -> finite.leq.(index_of x).(index_of y));
            join =
              (fun x y ->
                finite.element_values.(finite.join.(index_of x).(index_of y)));
            meet =
              (fun x y ->
                finite.element_values.(finite.meet.(index_of x).(index_of y)));
            sub =
              (fun x y ->
                finite.element_values.(sub.(index_of x).(index_of y)));
            imply =
              (fun x y ->
                finite.element_values.(imply.(index_of x).(index_of y)))
          }
        | Model.Product product ->
          let effective_fields =
            Array.of_list
              (List.map
                 (fun (field : Model.field) ->
                   let field_expr =
                     if expr.opposite then Model.flip_expr field.ty else field.ty
                   in
                   field, resolve field_expr)
                 product.fields)
          in
          let arity = Array.length effective_fields in
          let dims =
            Array.map (fun (_, semantics) -> Array.length semantics.values) effective_fields
          in
          let total =
            Array.fold_left (fun acc dim -> if dim = 0 then 0 else acc * dim) 1 dims
          in
          let decode ordinal =
            let coords = Array.make arity 0 in
            let remainder = ref ordinal in
            for i = arity - 1 downto 0 do
              let dim = dims.(i) in
              coords.(i) <- !remainder mod dim;
              remainder := !remainder / dim
            done;
            coords
          in
          let coords = Array.init total decode in
          let pack op_from_coords coord =
            let packed = ref 0 in
            for i = 0 to arity - 1 do
              let field, field_semantics = effective_fields.(i) in
              let field_value = op_from_coords field_semantics coord.(i) in
              packed :=
                !packed lor ((field_value land field.raw_mask) lsl field.shift)
            done;
            !packed
          in
          let values = Array.map (pack (fun semantics idx -> semantics.values.(idx))) coords in
          let unpack value index =
            let field, field_semantics = effective_fields.(index) in
            let field_value = (value lsr field.shift) land field.raw_mask in
            let values = field_semantics.values in
            let found = ref None in
            let i = ref 0 in
            while !i < Array.length values && !found = None do
              if values.(!i) = field_value then found := Some values.(!i);
              incr i
            done;
            match !found with
            | Some field_value -> field_value
            | None ->
              invalid_arg
                (Printf.sprintf
                   "invalid packed field value %d for %s"
                   field_value
                   field.name)
          in
          let pack_binary left right combine =
            let packed = ref 0 in
            for i = 0 to arity - 1 do
              let field, field_semantics = effective_fields.(i) in
              let left = unpack left i in
              let right = unpack right i in
              let value = combine field_semantics left right in
              packed :=
                !packed lor ((value land field.raw_mask) lsl field.shift)
            done;
            !packed
          in
          { values;
            bottom =
              pack
                (fun semantics _ -> semantics.bottom)
                (Array.make arity 0);
            top =
              pack
                (fun semantics _ -> semantics.top)
                (Array.make arity 0);
            leq =
              (fun left right ->
                let ok = ref true in
                for i = 0 to arity - 1 do
                  let _, field_semantics = effective_fields.(i) in
                  ok := !ok && field_semantics.leq (unpack left i) (unpack right i)
                done;
                !ok);
            join = (fun left right -> pack_binary left right (fun s -> s.join));
            meet = (fun left right -> pack_binary left right (fun s -> s.meet));
            sub = (fun left right -> pack_binary left right (fun s -> s.sub));
            imply = (fun left right -> pack_binary left right (fun s -> s.imply))
          }
      in
      Hashtbl.add cache expr semantics;
      semantics
  in
  resolve

let extract_lat_blocks source =
  if contains_substring source "<ocaml>"
  then failf "<ocaml> blocks are not supported in const-only observable mode";
  if contains_substring source "<ocaml "
  then failf "<ocaml> blocks are not supported in const-only observable mode";
  let source = strip_generated_blocks source in
  let len = String.length source in
  let rec loop pos index acc =
    match String.index_from_opt source pos '<' with
    | None -> List.rev acc
    | Some i ->
      if i + 4 <= len && String.sub source i 4 = "<lat"
      then (
        let tag_end =
          match String.index_from_opt source i '>' with
          | Some j -> j
          | None -> failf "unterminated <lat> tag"
        in
        let attrs_raw = String.sub source (i + 4) (tag_end - i - 4) in
        let close = "</lat>" in
        let rec find_body_end j =
          match String.index_from_opt source j '<' with
          | None -> failf "unterminated <lat> block"
          | Some k ->
            if k + String.length close <= len
               && String.sub source k (String.length close) = close
            then k
            else find_body_end (k + 1)
        in
        let body_end = find_body_end (tag_end + 1) in
        let close_end = body_end + String.length close in
        let body = String.sub source (tag_end + 1) (body_end - tag_end - 1) in
        let attrs = parse_attrs attrs_raw in
        loop close_end (index + 1) ({ index; attrs; body } :: acc))
      else loop (i + 1) index acc
  in
  loop 0 1 []

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

let ref_suffix module_name = String.lowercase_ascii module_name

let ref_values_var module_name = "ref_values_" ^ ref_suffix module_name

let test_values_var module_name = "test_values_" ^ ref_suffix module_name

let ref_name_fn module_name = "ref_name_" ^ ref_suffix module_name

let ref_bottom_var module_name = "ref_bottom_" ^ ref_suffix module_name

let ref_top_var module_name = "ref_top_" ^ ref_suffix module_name

let ref_leq_fn module_name = "ref_leq_" ^ ref_suffix module_name

let ref_join_fn module_name = "ref_join_" ^ ref_suffix module_name

let ref_meet_fn module_name = "ref_meet_" ^ ref_suffix module_name

let ref_sub_fn module_name = "ref_sub_" ^ ref_suffix module_name

let ref_imply_fn module_name = "ref_imply_" ^ ref_suffix module_name

let emit_int_array buf name values =
  Printf.bprintf buf "let %s = [|" name;
  Array.iteri
    (fun i value ->
      if i > 0 then Buffer.add_string buf "; ";
      Printf.bprintf buf "%d" value)
    values;
  Buffer.add_string buf "|]\n"

let emit_match_on_int buf ~indent name cases fallback =
  Printf.bprintf buf "let %s = function\n" name;
  List.iter
    (fun (lhs, rhs) ->
      Printf.bprintf buf "%s| %s -> %s\n" indent lhs rhs)
    cases;
  Printf.bprintf buf "%s| _ -> %s\n" indent fallback

let emit_match_on_pair buf ~indent name cases fallback =
  Printf.bprintf buf "let %s x y = match x, y with\n" name;
  List.iter
    (fun (lhs, rhs) ->
      Printf.bprintf buf "%s| %s -> %s\n" indent lhs rhs)
    cases;
  Printf.bprintf buf "%s| _ -> %s\n" indent fallback

let emit_reference_defs buf model =
  let resolve_semantics = make_semantics_resolver model in
  let emit_expr module_name (expr : Model.lattice_expr) =
    let semantics = resolve_semantics expr in
    let module_suffix = ref_suffix module_name in
    let _ = module_suffix in
    emit_int_array buf (ref_values_var module_name) semantics.values;
    (match Model.String_map.find expr.name model.Model.lattices with
     | Model.Base base ->
       let finite = base_finite base ~opposite:expr.opposite in
       let sub = finite_sub_table finite in
       let imply = finite_imply_table finite in
       emit_match_on_int
         buf
         ~indent:"  "
         (ref_name_fn module_name)
         (Array.to_list
            (Array.mapi
               (fun i value ->
                 string_of_int finite.element_values.(i), Printf.sprintf "%S" value)
               finite.element_names))
         "invalid_arg \"unknown lattice element\"";
       Printf.bprintf buf "let %s = %d\n" (ref_bottom_var module_name) semantics.bottom;
       Printf.bprintf buf "let %s = %d\n" (ref_top_var module_name) semantics.top;
       emit_match_on_pair
         buf
         ~indent:"  "
         (ref_leq_fn module_name)
         (List.concat_map
            (fun i ->
              List.init (Array.length finite.element_values) (fun j ->
                  Printf.sprintf "(%d, %d)" finite.element_values.(i) finite.element_values.(j),
                  string_of_bool finite.leq.(i).(j)))
            (List.init (Array.length finite.element_values) Fun.id))
         "invalid_arg \"unknown lattice element\"";
       emit_match_on_pair
         buf
         ~indent:"  "
         (ref_join_fn module_name)
         (List.concat_map
            (fun i ->
              List.init (Array.length finite.element_values) (fun j ->
                  Printf.sprintf "(%d, %d)" finite.element_values.(i) finite.element_values.(j),
                  string_of_int finite.element_values.(finite.join.(i).(j))))
            (List.init (Array.length finite.element_values) Fun.id))
         "invalid_arg \"unknown lattice element\"";
       emit_match_on_pair
         buf
         ~indent:"  "
         (ref_meet_fn module_name)
         (List.concat_map
            (fun i ->
              List.init (Array.length finite.element_values) (fun j ->
                  Printf.sprintf "(%d, %d)" finite.element_values.(i) finite.element_values.(j),
                  string_of_int finite.element_values.(finite.meet.(i).(j))))
            (List.init (Array.length finite.element_values) Fun.id))
         "invalid_arg \"unknown lattice element\"";
       emit_match_on_pair
         buf
         ~indent:"  "
         (ref_sub_fn module_name)
         (List.concat_map
            (fun i ->
              List.init (Array.length finite.element_values) (fun j ->
                  Printf.sprintf "(%d, %d)" finite.element_values.(i) finite.element_values.(j),
                  string_of_int finite.element_values.(sub.(i).(j))))
            (List.init (Array.length finite.element_values) Fun.id))
         "invalid_arg \"unknown lattice element\"";
       emit_match_on_pair
         buf
         ~indent:"  "
         (ref_imply_fn module_name)
         (List.concat_map
            (fun i ->
              List.init (Array.length finite.element_values) (fun j ->
                  Printf.sprintf "(%d, %d)" finite.element_values.(i) finite.element_values.(j),
                  string_of_int finite.element_values.(imply.(i).(j))))
            (List.init (Array.length finite.element_values) Fun.id))
         "invalid_arg \"unknown lattice element\""
     | Model.Product product ->
       let effective_fields =
         List.map
           (fun (field : Model.field) ->
             let field_expr =
               if expr.opposite then Model.flip_expr field.ty else field.ty
             in
             field, field_expr, Model.module_name_of_expr field_expr)
           product.fields
       in
       emit_int_array buf (ref_values_var module_name) semantics.values;
       Printf.bprintf buf "let %s x =\n" (ref_name_fn module_name);
       Printf.bprintf buf "  let b = Buffer.create 64 in\n";
       Printf.bprintf buf "  Buffer.add_string b %S;\n" (module_name ^ " { ");
       List.iteri
         (fun i ((field : Model.field), _, child_module_name) ->
           if i > 0 then Buffer.add_string buf "  Buffer.add_string b \"; \";\n";
           Printf.bprintf buf "  Buffer.add_string b %S;\n" (field.name ^ " = ");
           Printf.bprintf
             buf
             "  Buffer.add_string b (%s ((x lsr %d) land %d));\n"
             (ref_name_fn child_module_name)
             field.shift
             field.raw_mask)
         effective_fields;
       Buffer.add_string buf "  Buffer.add_string b \" }\";\n";
       Buffer.add_string buf "  Buffer.contents b\n";
       let render_pack_constant child_value_name =
         effective_fields
         |> List.mapi (fun i ((field : Model.field), _, child_module_name) ->
                let piece =
                  Printf.sprintf
                    "((%s land %d) lsl %d)"
                    (child_value_name child_module_name)
                    field.raw_mask
                    field.shift
                in
                if i = 0 then piece else "  lor " ^ piece)
         |> String.concat "\n"
       in
       Printf.bprintf buf "let %s =\n%s\n" (ref_bottom_var module_name)
         (render_pack_constant ref_bottom_var);
       Printf.bprintf buf "let %s =\n%s\n" (ref_top_var module_name)
         (render_pack_constant ref_top_var);
       let emit_product_op fn_name child_fn_name =
       Printf.bprintf buf "let %s x y =\n" fn_name;
        effective_fields
         |> List.iteri (fun i ((field : Model.field), _, child_module_name) ->
                let line =
                  Printf.sprintf
                    "((%s ((x lsr %d) land %d) ((y lsr %d) land %d) land %d) lsl %d)"
                    (child_fn_name child_module_name)
                    field.shift
                    field.raw_mask
                    field.shift
                    field.raw_mask
                    field.raw_mask
                    field.shift
                in
                if i = 0
                then Printf.bprintf buf "  %s\n" line
                else Printf.bprintf buf "  lor %s\n" line)
       in
       Printf.bprintf buf "let %s x y =\n" (ref_leq_fn module_name);
       List.iteri
         (fun i ((field : Model.field), _, child_module_name) ->
           let clause =
             Printf.sprintf
               "%s ((x lsr %d) land %d) ((y lsr %d) land %d)"
               (ref_leq_fn child_module_name)
               field.shift
               field.raw_mask
               field.shift
               field.raw_mask
           in
           if i = 0
           then Printf.bprintf buf "  %s\n" clause
           else Printf.bprintf buf "  && %s\n" clause)
         effective_fields;
       emit_product_op (ref_join_fn module_name) ref_join_fn;
       emit_product_op (ref_meet_fn module_name) ref_meet_fn;
       emit_product_op (ref_sub_fn module_name) ref_sub_fn;
       emit_product_op (ref_imply_fn module_name) ref_imply_fn);
    Buffer.add_char buf '\n'
  in
  List.iter
    (fun expr -> emit_expr (Model.module_name_of_expr expr) expr)
    (Model.emitted_module_exprs model);
  List.iter
    (fun expr ->
      let module_name = Model.module_name_of_expr expr in
      Printf.bprintf
        buf
        "let %s = Array.map %s.Repr.from_int_unsafe %s\n"
        (test_values_var module_name)
        module_name
        (ref_values_var module_name))
    (Model.emitted_module_exprs model)

let emit_test_runtime buf =
  Buffer.add_string
    buf
    {|let exhaustive_threshold = 256
let repr_exhaustive_threshold = 4096
let sample_count = 200
let rng = Random.State.make [| 0x51ed; 0x1234; 0x2026 |]

let fail fmt = Printf.ksprintf failwith fmt

let ensure cond fmt =
  Printf.ksprintf (fun message -> if not cond then fail "%s" message) fmt

let safe_product limit a b =
  if a = 0 || b = 0
  then 0
  else if a > limit / b
  then limit + 1
  else a * b

let should_exhaust sizes =
  List.fold_left (safe_product exhaustive_threshold) 1 sizes
  <= exhaustive_threshold

let iter1 values f =
  let n = Array.length values in
  if should_exhaust [ n ]
  then Array.iter f values
  else
    for _ = 1 to sample_count do
      f values.(Random.State.int rng n)
    done

let iter2 xs ys f =
  let nx = Array.length xs in
  let ny = Array.length ys in
  if should_exhaust [ nx; ny ]
  then (
    Array.iter (fun x -> Array.iter (fun y -> f x y) ys) xs)
  else
    for _ = 1 to sample_count do
      f xs.(Random.State.int rng nx) ys.(Random.State.int rng ny)
    done

let iter3 xs ys zs f =
  let nx = Array.length xs in
  let ny = Array.length ys in
  let nz = Array.length zs in
  if should_exhaust [ nx; ny; nz ]
  then (
    Array.iter
      (fun x -> Array.iter (fun y -> Array.iter (fun z -> f x y z) zs) ys)
      xs)
  else
    for _ = 1 to sample_count do
      f
        xs.(Random.State.int rng nx)
        ys.(Random.State.int rng ny)
        zs.(Random.State.int rng nz)
    done

let array_mem xs x =
  let found = ref false in
  Array.iter (fun y -> if y = x then found := true) xs;
  !found

let check_public_repr ~name ~valid_reprs ~to_int_unsafe ~from_int_unsafe =
  Array.iter
    (fun value ->
      ensure (to_int_unsafe (from_int_unsafe value) = value)
        "%s: unsafe int roundtrip mismatch on valid repr %d"
        name value)
    valid_reprs;
  ()

let check_base_lattice
    ~name
    ~values
    ~valid_reprs
    ~to_int_unsafe
    ~from_int_unsafe
    ~view
    ~of_view
    ~min
    ~max
    ~le
    ~equal
    ~join
    ~meet
    ~subtract
    ~imply
    ~print
    ~show
    ~name_fn
    ~of_name
    ~legacy
    ~ref_min
    ~ref_max
    ~ref_le
    ~ref_join
    ~ref_meet
    ~ref_subtract
    ~ref_imply
    ~ref_name
  =
  check_public_repr ~name ~valid_reprs ~to_int_unsafe ~from_int_unsafe;
  ensure (to_int_unsafe min = ref_min) "%s: min mismatch" name;
  ensure (to_int_unsafe max = ref_max) "%s: max mismatch" name;
  ensure (array_mem values legacy) "%s: legacy is invalid" name;
  iter1 values
    (fun x ->
      let x_i = to_int_unsafe x in
      let rendered = name_fn x in
      let expected_name = ref_name x_i in
      ensure (of_view (view x) = x)
        "%s: of_view/view mismatch for %d"
        name x_i;
      ensure (rendered = expected_name)
        "%s: name mismatch for %d"
        name x_i;
      ensure (show x = expected_name)
        "%s: show mismatch for %d"
        name x_i;
      ensure (Format.asprintf "%a" print x = expected_name)
        "%s: print mismatch for %d"
        name x_i;
      match of_name expected_name with
      | Some y ->
        ensure (equal y x)
          "%s: of_name mismatch for %S"
          name expected_name
      | None -> fail "%s: of_name failed for %S" name expected_name);
  iter2 values values
    (fun x y ->
      let x_i = to_int_unsafe x in
      let y_i = to_int_unsafe y in
      ensure (equal x y = (x_i = y_i))
        "%s: equal mismatch for %d and %d"
        name x_i y_i;
      ensure (le x y = ref_le x_i y_i)
        "%s: le mismatch for %d and %d"
        name x_i y_i;
      ensure (to_int_unsafe (join x y) = ref_join x_i y_i)
        "%s: join mismatch for %d and %d"
        name x_i y_i;
      ensure (to_int_unsafe (meet x y) = ref_meet x_i y_i)
        "%s: meet mismatch for %d and %d"
        name x_i y_i;
      ensure (to_int_unsafe (subtract x y) = ref_subtract x_i y_i)
        "%s: subtract mismatch for %d and %d"
        name x_i y_i;
      ensure (to_int_unsafe (imply x y) = ref_imply x_i y_i)
        "%s: imply mismatch for %d and %d"
        name x_i y_i);
  iter3 values values values
    (fun x y z ->
      let join_yz = join y z in
      let join_xy = join x y in
      let meet_yz = meet y z in
      let meet_xy = meet x y in
      let sub_xy = subtract x y in
      let imply_xy = imply x y in
      ensure (join x (join y z) = join (join x y) z)
        "%s: join associativity failed"
        name;
      ensure (meet x (meet y z) = meet (meet x y) z)
        "%s: meet associativity failed"
        name;
      ensure ((le sub_xy z) = (le x join_yz))
        "%s: subtraction residuation failed"
        name;
      ensure ((le z imply_xy) = (le (meet z x) y))
        "%s: implication residuation failed"
        name)

let check_product_lattice
    ~name
    ~values
    ~valid_reprs
    ~to_int_unsafe
    ~from_int_unsafe
    ~min
    ~max
    ~le
    ~equal
    ~join
    ~meet
    ~subtract
    ~imply
    ~print
    ~show
    ~name_fn
    ~ref_min
    ~ref_max
    ~ref_le
    ~ref_join
    ~ref_meet
    ~ref_subtract
    ~ref_imply
    ~ref_name
  =
  check_public_repr ~name ~valid_reprs ~to_int_unsafe ~from_int_unsafe;
  ensure (to_int_unsafe min = ref_min) "%s: min mismatch" name;
  ensure (to_int_unsafe max = ref_max) "%s: max mismatch" name;
  iter1 values
    (fun x ->
      let x_i = to_int_unsafe x in
      let rendered = name_fn x in
      let expected_name = ref_name x_i in
      ensure (rendered = expected_name)
        "%s: name mismatch for %d"
        name x_i;
      ensure (show x = expected_name)
        "%s: show mismatch for %d"
        name x_i;
      ensure (Format.asprintf "%a" print x = expected_name)
        "%s: print mismatch for %d"
        name x_i);
  iter2 values values
    (fun x y ->
      let x_i = to_int_unsafe x in
      let y_i = to_int_unsafe y in
      ensure (equal x y = (x_i = y_i))
        "%s: equal mismatch for %d and %d"
        name x_i y_i;
      ensure (le x y = ref_le x_i y_i)
        "%s: le mismatch for %d and %d"
        name x_i y_i;
      ensure (to_int_unsafe (join x y) = ref_join x_i y_i)
        "%s: join mismatch for %d and %d"
        name x_i y_i;
      ensure (to_int_unsafe (meet x y) = ref_meet x_i y_i)
        "%s: meet mismatch for %d and %d"
        name x_i y_i;
      ensure (to_int_unsafe (subtract x y) = ref_subtract x_i y_i)
        "%s: subtract mismatch for %d and %d"
        name x_i y_i;
      ensure (to_int_unsafe (imply x y) = ref_imply x_i y_i)
        "%s: imply mismatch for %d and %d"
        name x_i y_i);
  iter3 values values values
    (fun x y z ->
      let join_yz = join y z in
      let sub_xy = subtract x y in
      let imply_xy = imply x y in
      ensure (join x (join y z) = join (join x y) z)
        "%s: join associativity failed"
        name;
      ensure (meet x (meet y z) = meet (meet x y) z)
        "%s: meet associativity failed"
        name;
      ensure ((le sub_xy z) = (le x join_yz))
        "%s: subtraction residuation failed"
        name;
      ensure ((le z imply_xy) = (le (meet z x) y))
        "%s: implication residuation failed"
        name)

let check_morph ~name ~inputs ~expected ~to_int_unsafe ~apply =
  Array.iteri
    (fun i x ->
      ensure (to_int_unsafe (apply x) = expected.(i))
        "%s: mismatch on input %d"
        name i)
    inputs
|}

let emit_lattice_checks buf model =
  List.iter
    (fun (expr : Model.lattice_expr) ->
      let module_name = Model.module_name_of_expr expr in
      let lattice = Model.String_map.find expr.Model.name model.Model.lattices in
      let is_base = match lattice with Model.Base _ -> true | Product _ -> false in
      if is_base
      then
        Printf.bprintf
          buf
          "let () =\n\
           \  check_base_lattice\n\
           \    ~name:%S\n\
           \    ~values:%s\n\
           \    ~valid_reprs:%s\n\
           \    ~to_int_unsafe:%s.Repr.to_int_unsafe\n\
           \    ~from_int_unsafe:%s.Repr.from_int_unsafe\n\
           \    ~view:%s.view\n\
           \    ~of_view:%s.of_view\n\
           \    ~min:%s.min\n\
           \    ~max:%s.max\n\
           \    ~le:%s.le\n\
           \    ~equal:%s.equal\n\
           \    ~join:%s.join\n\
           \    ~meet:%s.meet\n\
           \    ~subtract:%s.subtract\n\
           \    ~imply:%s.imply\n\
           \    ~print:%s.print\n\
           \    ~show:%s.show\n\
           \    ~name_fn:%s.name\n\
           \    ~of_name:%s.of_name\n\
           \    ~legacy:%s.legacy\n\
           \    ~ref_min:%s\n\
           \    ~ref_max:%s\n\
           \    ~ref_le:%s\n\
           \    ~ref_join:%s\n\
           \    ~ref_meet:%s\n\
           \    ~ref_subtract:%s\n\
           \    ~ref_imply:%s\n\
           \    ~ref_name:%s\n\n"
          module_name
          (test_values_var module_name)
          (ref_values_var module_name)
          module_name
          module_name
          module_name
          module_name
          module_name
          module_name
          module_name
          module_name
          module_name
          module_name
          module_name
          module_name
          module_name
          module_name
          module_name
          module_name
          module_name
          (ref_bottom_var module_name)
          (ref_top_var module_name)
          (ref_leq_fn module_name)
          (ref_join_fn module_name)
          (ref_meet_fn module_name)
          (ref_sub_fn module_name)
          (ref_imply_fn module_name)
          (ref_name_fn module_name)
      else
        Printf.bprintf
          buf
          "let () =\n\
           \  check_product_lattice\n\
           \    ~name:%S\n\
           \    ~values:%s\n\
           \    ~valid_reprs:%s\n\
           \    ~to_int_unsafe:%s.Repr.to_int_unsafe\n\
           \    ~from_int_unsafe:%s.Repr.from_int_unsafe\n\
           \    ~min:%s.min\n\
           \    ~max:%s.max\n\
           \    ~le:%s.le\n\
           \    ~equal:%s.equal\n\
           \    ~join:%s.join\n\
           \    ~meet:%s.meet\n\
           \    ~subtract:%s.subtract\n\
           \    ~imply:%s.imply\n\
           \    ~print:%s.print\n\
           \    ~show:%s.show\n\
           \    ~name_fn:%s.name\n\
           \    ~ref_min:%s\n\
           \    ~ref_max:%s\n\
           \    ~ref_le:%s\n\
           \    ~ref_join:%s\n\
           \    ~ref_meet:%s\n\
           \    ~ref_subtract:%s\n\
           \    ~ref_imply:%s\n\
           \    ~ref_name:%s\n\n"
          module_name
          (test_values_var module_name)
          (ref_values_var module_name)
          module_name
          module_name
          module_name
          module_name
          module_name
          module_name
          module_name
          module_name
          module_name
          module_name
          module_name
          module_name
          module_name
          (ref_bottom_var module_name)
          (ref_top_var module_name)
          (ref_leq_fn module_name)
          (ref_join_fn module_name)
          (ref_meet_fn module_name)
          (ref_sub_fn module_name)
          (ref_imply_fn module_name)
          (ref_name_fn module_name))
    (Model.emitted_module_exprs model)

let emit_product_checks buf model =
  let emit_one module_name (expr : Model.lattice_expr) (product : Model.product) =
    Printf.bprintf buf "let () =\n";
    Printf.bprintf buf "  iter1 %s (fun x ->\n" (test_values_var module_name);
    Printf.bprintf buf "    let view = %s.view x in\n" module_name;
    Printf.bprintf buf "    ensure (%s.of_view view = x) %S;\n"
      module_name
      (module_name ^ ": of_view/view mismatch");
    Printf.bprintf buf "    let rebuilt = %s.make\n" module_name;
    List.iter
      (fun (field : Model.field) ->
        Printf.bprintf buf "      ~%s:view.%s\n" field.name field.name)
      product.fields;
    Buffer.add_string buf "    in\n";
    Printf.bprintf
      buf
      "    ensure (rebuilt = x) %S\n"
      (module_name ^ ": make/view mismatch");
    Buffer.add_string buf "  );\n";
    List.iter
      (fun (field : Model.field) ->
        let field_expr =
          if expr.opposite then Model.flip_expr field.ty else field.ty
        in
        let field_module_name = Model.module_name_of_expr field_expr in
        let proj_name = "proj_" ^ field.name in
        let with_name = "with_" ^ field.name in
        let min_with_name = "min_with_" ^ field.name in
        let max_with_name = "max_with_" ^ field.name in
        Printf.bprintf buf "  iter2 %s %s (fun x value ->\n"
          (test_values_var module_name)
          (test_values_var field_module_name);
        Printf.bprintf
          buf
          "    ensure (%s.%s (%s.%s value x) = value) %S;\n"
          module_name
          proj_name
          module_name
          with_name
          (module_name ^ ": proj/with roundtrip mismatch on " ^ field.name);
        Printf.bprintf
          buf
          "    ensure (%s.%s (%s.%s value) = value) %S;\n"
          module_name
          proj_name
          module_name
          min_with_name
          (module_name ^ ": proj/min_with mismatch on " ^ field.name);
        Printf.bprintf
          buf
          "    ensure (%s.%s (%s.%s value) = value) %S;\n"
          module_name
          proj_name
          module_name
          max_with_name
          (module_name ^ ": proj/max_with mismatch on " ^ field.name);
        Printf.bprintf
          buf
          "    let expected = %s.of_view { (%s.view x) with %s = value } in\n"
          module_name
          module_name
          field.name;
        Printf.bprintf
          buf
          "    ensure (%s.%s value x = expected) %S\n"
          module_name
          with_name
          (module_name ^ ": with mismatch on " ^ field.name);
        Buffer.add_string buf "  );\n")
      product.fields;
    Buffer.add_string buf "  ()\n\n"
  in
  List.iter
    (fun (expr : Model.lattice_expr) ->
      match Model.String_map.find expr.name model.Model.lattices with
      | Model.Base _ -> ()
      | Model.Product product ->
        emit_one (Model.module_name_of_expr expr) expr product)
    (Model.emitted_module_exprs model)

let emit_morph_checks buf model =
  let resolve_semantics = make_semantics_resolver model in
  Model.String_map.iter
    (fun _ morph ->
      let core = Model.morph_core_of morph in
      let source_module = Model.module_name_of_expr core.source in
      let target_module = Model.module_name_of_expr core.target in
      let target_semantics = resolve_semantics core.target in
      let expected =
        Array.map (fun index -> target_semantics.values.(index)) core.map
      in
      let expected_var = "ref_expected_" ^ ref_suffix core.name in
      emit_int_array buf expected_var expected;
      Printf.bprintf
        buf
        "let () = check_morph ~name:%S ~inputs:%s ~expected:%s ~to_int_unsafe:%s.Repr.to_int_unsafe ~apply:%s\n\n"
        core.name
        (test_values_var source_module)
        expected_var
        target_module
        core.name)
    model.Model.morphs

let render_test_main model =
  let buf = Buffer.create 32768 in
  Buffer.add_string buf "open Generated\n\n";
  emit_test_runtime buf;
  Buffer.add_char buf '\n';
  emit_reference_defs buf model;
  emit_lattice_checks buf model;
  emit_product_checks buf model;
  emit_morph_checks buf model;
  Buffer.contents buf

let run_block_tests path (block : lat_block) =
  let input_name =
    Printf.sprintf "%s#lat%d" (Filename.basename path) block.index
  in
  let ast = Parse.from_string ~input_name block.body in
  let model = Model.resolve ast in
  let outputs =
    Generate.render_string
      ?root_module:block.attrs.root_module
      ~input_name
      block.body
  in
  let main = render_test_main model in
  with_temp_dir "observable-test-" (fun dir ->
      write_file (Filename.concat dir "generated.ml") outputs.ml;
      write_file (Filename.concat dir "generated.mli") outputs.mli;
      write_file (Filename.concat dir "main.ml") main;
      run_command ~cwd:dir "ocamlc -c generated.mli";
      run_command ~cwd:dir "ocamlc -c generated.ml";
      run_command ~cwd:dir "ocamlc -c main.ml";
      run_command ~cwd:dir "ocamlc -o main.exe generated.cmo main.cmo";
      run_command ~cwd:dir "./main.exe")

let test_lattice_source ?root_module ~input_name source =
  let ast = Parse.from_string ~input_name source in
  let model = Model.resolve ast in
  let outputs = Generate.render_string ?root_module ~input_name source in
  with_temp_dir "observable-source-test-" (fun dir ->
      write_file (Filename.concat dir "generated.ml") outputs.ml;
      write_file (Filename.concat dir "generated.mli") outputs.mli;
      write_file (Filename.concat dir "main.ml") (render_test_main model);
      run_command ~cwd:dir "ocamlc -c generated.mli";
      run_command ~cwd:dir "ocamlc -c generated.ml";
      run_command ~cwd:dir "ocamlc -c main.ml";
      run_command ~cwd:dir "ocamlc -o main.exe generated.cmo main.cmo";
      run_command ~cwd:dir "./main.exe")

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

let process_file ~mode ~test path =
  let source = read_file path in
  let updated = update_source source in
  let blocks = if test then extract_lat_blocks source else [] in
  (match mode with
  | Update -> if updated <> source then write_file path updated
  | Check ->
    if updated <> source then failf "observable snapshot is stale: %s" path);
  List.iter (run_block_tests path) blocks

let rec process_path ~mode ~test path =
  if Sys.is_directory path
  then
    Sys.readdir path
    |> Array.to_list
    |> List.sort String.compare
    |> List.iter (fun entry -> process_path ~mode ~test (Filename.concat path entry))
  else if Filename.check_suffix path ".html"
  then process_file ~mode ~test path
  else ()

let process_paths ~mode ~test paths = List.iter (process_path ~mode ~test) paths
