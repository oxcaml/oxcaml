(* TEST
 flags = "-I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/parsing -I ${ocamlsrcdir}/driver";
 include ocamlcommon;
 expect;
*)

module Diagnostic = Structured_diagnostic
module Block = Diagnostic.Block
module Entities = Diagnostic.Entities
module Glossary = Diagnostic.Glossary
module Inline = Diagnostic.Inline

let file = "test.ml"

let context : Mode_diagnostics.context =
  { inclusion_site_at = (fun _ -> None);
    declared_modalities_at = (fun _ ~argument:_ -> None);
    constructor_arguments_at = (fun _ _ -> None);
    documentation =
      { of_mode = (fun _ -> None); of_modality = (fun _ -> None) }
  }

let typer_error text =
  let lexbuf = Lexing.from_string text in
  Location.init lexbuf file;
  Location.input_name := file;
  Compmisc.init_path ();
  match Parse.implementation lexbuf with
  | structure -> (
    match Typemod.type_structure (Compmisc.initial_env ()) structure with
    | _ -> None
    | exception exn -> Some exn)
  | exception exn -> Some exn

let inlines_to_string inlines =
  let buffer = Buffer.create 128 in
  let rec add (inline : Inline.t) =
    match inline with
    | Text text -> Buffer.add_string buffer text
    | Annotated { annotation; content } -> (
      match annotation with
      | Code ->
        Buffer.add_char buffer '`';
        List.iter add content;
        Buffer.add_char buffer '`'
      | Source _ ->
        Buffer.add_char buffer '[';
        List.iter add content;
        Buffer.add_char buffer ']'
      | Mention { entity; form } ->
        Printf.bprintf buffer "{%d%s:" (Entities.Id.to_int entity)
          (match form with
          | Name -> ""
          | Pronoun -> "*");
        List.iter add content;
        Buffer.add_char buffer '}'
      | Term id ->
        List.iter add content;
        Printf.bprintf buffer "#%d" (Glossary.Id.to_int id))
  in
  List.iter add inlines;
  Buffer.contents buffer

let kind_marker (kind : Diagnostic.Kind.t) =
  match kind with
  | Explanation -> "-"
  | Background -> "="
  | Suggestion -> "+"

let relation_marker (relation : Diagnostic.Relation.t) =
  match relation with
  | Claim -> ""
  | Elaboration -> "~"

let rec print_block ~depth ~relation (block : Block.t) =
  Format.printf "%s%s%s %s@."
    (String.make (depth * 2) ' ')
    relation (kind_marker block.kind)
    (inlines_to_string block.content);
  List.iter
    (fun ((relation : Diagnostic.Relation.t), child) ->
      print_block ~depth:(depth + 1) ~relation:(relation_marker relation) child)
    block.children

let print_diagnostic (diagnostic : Diagnostic.t) =
  Format.printf "title: %s@." diagnostic.title;
  List.iter (print_block ~depth:0 ~relation:"") diagnostic.body

let print_entities (diagnostic : Diagnostic.t) =
  List.iter
    (fun (id, (loc : Location.t)) ->
      Format.printf "entity %d: %s %d-%d@." (Entities.Id.to_int id)
        loc.loc_start.pos_fname loc.loc_start.pos_cnum loc.loc_end.pos_cnum)
    (Entities.to_list diagnostic.entities)

let print_glossary (diagnostic : Diagnostic.t) =
  List.iter
    (fun (id, (entry : Glossary.Entry.t)) ->
      Format.printf "term %d: %s (%s)@." (Glossary.Id.to_int id) entry.term
        entry.category)
    (Glossary.to_list diagnostic.glossary)

let diagnose ?(pronouns = Mode_diagnostics.Pronouns.Use_pronouns)
    ?(source_file = file) text =
  match typer_error text with
  | None -> None
  | Some exn -> (
    let loc =
      match Location.error_of_exn exn with
      | Some (`Ok report) -> report.Location.main.loc
      | Some `Already_displayed | None -> Location.none
    in
    let source = Mode_diagnostics.Source.create ~file:source_file ~text in
    match Mode_diagnostics.error ~source ~context ~pronouns ~loc exn with
    | diagnostic -> diagnostic
    | exception exn ->
      Format.printf "diagnosis raised: %s@." (Printexc.to_string exn);
      None)

let show ?pronouns ?source_file text =
  match diagnose ?pronouns ?source_file text with
  | None -> Format.printf "no diagnostic@."
  | Some diagnostic -> print_diagnostic diagnostic

let nonportable_capture =
  {|
let test () =
  let x = ref 42 in
  let foo () = x := 24 in
  let (bar @ portable) () = let _ = foo in () in
  ignore bar
|}

let () = show nonportable_capture

[%%expect {|
module Diagnostic = Structured_diagnostic
module Block = Diagnostic.Block
module Entities = Diagnostic.Entities
module Glossary = Diagnostic.Glossary
module Inline = Diagnostic.Inline
val file : string = "test.ml"
val context : Mode_diagnostics.context =
  {Mode_diagnostics.inclusion_site_at = <fun>; declared_modalities_at = <fun>;
   constructor_arguments_at = <fun>;
   documentation =
    {Mode_diagnostics.Documentation.of_mode = <fun>; of_modality = <fun>}}
val typer_error : string -> exn option = <fun>
val inlines_to_string : Inline.t list -> string = <fun>
val kind_marker : Diagnostic.Kind.t -> string = <fun>
val relation_marker : Diagnostic.Relation.t -> string = <fun>
val print_block : depth:int -> relation:string -> Block.t -> unit = <fun>
val print_diagnostic : Diagnostic.t -> unit = <fun>
val print_entities : Diagnostic.t -> unit = <fun>
val print_glossary : Diagnostic.t -> unit = <fun>
val diagnose :
  ?pronouns:Mode_diagnostics.Pronouns.t ->
  ?source_file:string -> string -> Structured_diagnostic.t option = <fun>
val show :
  ?pronouns:Mode_diagnostics.Pronouns.t ->
  ?source_file:string -> string -> unit = <fun>
val nonportable_capture : string =
  "\nlet test () =\n  let x = ref 42 in\n  let foo () = x := 24 in\n  let (bar @ portable) () = let _ = foo in () in\n  ignore bar\n"
title: Explain mode error (portability)
-
  - {0:`foo`} is expected to be `portable`#0.
    ~- {1:The function `bar`}[ is annotated as `portable`#0].
      ~- {1*:It} closes over {0:`foo`}.
        ~- Therefore, [`foo`] is also expected to be `portable`#0.
  - But {0:`foo`} is `nonportable`#1.
    ~- {0:`foo`} uses {2:`x`} as `uncontended`#2 data.
      ~- The signature of {3:`:=`} requires {2:`x`} to be `uncontended`#2.
    ~= A function that closes over `uncontended`#2 data is `nonportable`#1.
|}]

let () = show ~pronouns:Names_only nonportable_capture

[%%expect {|
title: Explain mode error (portability)
-
  - {0:`foo`} is expected to be `portable`#0.
    ~- {1:The function `bar`}[ is annotated as `portable`#0].
      ~- {1:The function `bar`} closes over {0:`foo`}.
        ~- Therefore, [`foo`] is also expected to be `portable`#0.
  - But {0:`foo`} is `nonportable`#1.
    ~- {0:`foo`} uses {2:`x`} as `uncontended`#2 data.
      ~- The signature of {3:`:=`} requires {2:`x`} to be `uncontended`#2.
    ~= A function that closes over `uncontended`#2 data is `nonportable`#1.
|}]

let () =
  match diagnose nonportable_capture with
  | None -> Format.printf "no diagnostic@."
  | Some diagnostic ->
    print_entities diagnostic;
    print_glossary diagnostic

[%%expect {|
entity 0: test.ml 97-100
entity 1: test.ml 84-106
entity 2: test.ml 50-51
entity 3: test.ml 52-54
term 0: portable (Mode)
term 1: nonportable (Mode)
term 2: uncontended (Mode)
|}]

let () = show ~source_file:"elsewhere.ml" nonportable_capture

[%%expect {|
title: Explain mode error (portability)
-
  - {0:This identifier} is expected to be `portable`#0.
    ~- {1:The function} is expected to be `portable`#0.
      ~- {1*:It} closes over {0:this identifier}.
        ~- Therefore, [this identifier] is also expected to be `portable`#0.
  - But {0:this identifier} is `nonportable`#1.
    ~- {0:This identifier} uses {2:this identifier} as `uncontended`#2 data.
      ~- The signature of {3:this identifier} requires {2:this identifier} to be `uncontended`#2.
    ~= A function that closes over `uncontended`#2 data is `nonportable`#1.
|}]

let () =
  show
    {|
let escape () =
  let x = stack_ (ref 42) in
  x
|}

[%%expect {|
title: Explain mode error (locality)
-
  - {0:`x`} is expected to be `local` to the parent region#0 or `global`#1.
    ~- {0*:It}'s returned.
    ~+ Use `exclave_` to return a `local`#2 value.
  - But {0:`x`} is `local`#2.
|}]

let () =
  show
    {|
module M : sig
  val baz : unit -> unit @@ portable
end = struct
  let x = ref 42
  let foo () = x := 24
  let bar () = let _ = foo in ()
  let baz () = let _ = bar in ()
end
|}

[%%expect {|
title: Explain mode error (portability)
- {0:The module} does not match its signature.
  -
    - {1:`baz`} is expected to be `portable`#0.
      ~- [The signature requires {1*:it} to be `portable`#0].
    - But {1*:it}'s `nonportable`#1.
      ~- {1:`baz`} uses {2:`bar`}.
        ~- [`bar`] is `nonportable`#1.
          ~- {2:`bar`} uses {3:`foo`}.
            ~- [`foo`] is `nonportable`#1.
              ~- {3:`foo`} uses {4:`x`} as `uncontended`#2 data.
                ~- The signature of {5:`:=`} requires {4:`x`} to be `uncontended`#2.
      ~= A function that closes over `uncontended`#2 data is `nonportable`#1.
|}]

let () = show {|let dup x = ((x, x) : @ unique)|}

[%%expect {|
title: Explain mode error
-
  - [This value is used here].
  - But it is also being [used as ]`unique`#0.
    ~= A value used as `unique`#0 must have no other use: that is what `unique`#0 means.
|}]

let () = show {|let dup (x @ once) = (x, x)|}

[%%expect {|
title: Explain mode error
-
  - [This value is used here].
  - But it is `once`#0 and is also being [used].
    ~= A `once`#0 value may be used at most once.
|}]

let () = show {|type t : immutable_data = { mutable a : int }|}

[%%expect {|
title: Explain mode error
-
  - [Type t does not cross the contention, visibility axes].
  - But the kind it is checked against requires it to.
    ~= A `mod` annotation claims a type's values may be used at the stronger mode on those axes, whatever mode they are held at.
|}]

let () = show {|type t = int [@@unsafe_allow_any_mode_crossing]|}

[%%expect {|
title: Explain mode error
-
  - [This declaration is marked ]`[@@unsafe_allow_any_mode_crossing]`.
  - But the attribute applies only to records, unboxed products and variants.
    ~= The attribute overrides the mode bounds computed from a type's fields or constructors; a type with neither has nothing to override.
|}]

let () =
  show
    {|
module M : sig
  type t = { global_ a : int }
end = struct
  type t = { a : int }
end
|}

[%%expect {|
title: Explain mode error (locality)
- {0:The module} does not match its signature.
  - The declarations of {1:type `t`} do not match.
    - The declarations of {2:the field `a`} disagree on locality.
      ~- {2*:It}['s `@@ global`#0 in the expected declaration].
      - {2:The field `a`}[ has no locality modality in the actual declaration].
        ~= Field and constructor-argument modalities must match exactly on both sides.
|}]

let () = show {|type t = { a : int [@atomic] }|}

[%%expect {|
title: Explain mode error
-
  - [`a`] is declared `[@atomic]` but is not mutable.
    ~= Atomicity describes how a field is written, so only a mutable field can be atomic.
    ~+ Add `mutable`, or drop the `[@atomic]`.
|}]

let () =
  show
    {|
module M : sig
  val f : unit -> unit @@ stateless
end = struct
  let r = ref ()
  let f () = r.contents; r.contents <- ()
end
|}

[%%expect {|
title: Explain mode error (statefulness, portability)
- {0:The module} does not match its signature.
  -
    - {1:`f`} is expected to be `stateless`#0.
      ~- [The signature requires {1*:it} to be `stateless`#0].
    - But {1*:it}'s weaker than `writing`#1.
      ~- {1:`f`} uses {2:`r`} as `write`#2 data.
        ~- {2:`r`}'s mutable field `contents` is being written.
  -
    - {1:`f`} is expected to be `portable`#3.
      ~- [The signature requires {1*:it} to be `portable`#3].
    - But {1*:it}'s weaker than `corruptible`#4.
      ~- {1:`f`} uses {2:`r`} as `corrupted`#5 data.
        ~- {2:`r`}'s mutable field `contents` is being written.
      ~= A function that closes over `uncontended`#6 data is `nonportable`#7.
|}]

let () =
  show
    {|
module (F @ static) (X : sig end) = struct end
|}

[%%expect {|
title: Explain mode error (staticity)
-
  - {0:`(X : sig end) = struct end`} is expected to be `static`#0.
  - But {0*:it}'s `dynamic`#1.
    ~- {0:`(X : sig end) = struct end`} shares the staticity of {1:`X`}.
      ~- [`X`] is `dynamic`#1.
|}]

let () =
  show
    {|
module M = struct
  module F (X : sig end) = struct end
  module (Y @ static) = F(struct end)
end
|}

[%%expect {|
title: Explain mode error (staticity)
-
  - {0:`F(struct end)`} is expected to be `static`#0.
  - But {0*:it}'s `dynamic`#1.
    ~- {0:`F(struct end)`} is an application of {1:`F`}.
      ~- [`F`] is `dynamic`#1.
        ~- {1:`F`} shares the staticity of {2:`X`}.
          ~- [`X`] is `dynamic`#1.
|}]

let () = show {|let f () = stack_ 42|}

[%%expect {|
title: Explain mode error
-
  - [A literal is not allocated at runtime].
  - But `stack_` must be applied to something that allocates.
    ~= `stack_` chooses where an allocation happens, and this value needs no allocation to choose from.
    ~+ Remove the `stack_`.
|}]

let () = show {|let f (x : int) = x ^ "a"|}

[%%expect {|
no diagnostic
|}]

let () =
  show
    {|
module M : sig
  val f : unit -> unit @@ stateless
end = struct
  let r = ref ()
  let (g @ writing) = () and f () = ignore r.contents; r.contents <- ()
end
|}

[%%expect {|
title: Explain mode error (statefulness, portability)
- {0:The module} does not match its signature.
  -
    - {1:`f`} is expected to be `stateless`#0.
      ~- [The signature requires {1*:it} to be `stateless`#0].
    - But {1*:it}'s weaker than [the annotated ]`writing`#1.
      ~- {1:`f`} uses {2:`r`} as `write`#2 data.
        ~- {2:`r`}'s mutable field `contents` is being written.
  -
    - {1:`f`} is expected to be `portable`#3.
      ~- [The signature requires {1*:it} to be `portable`#3].
    - But {1*:it}'s weaker than `corruptible`#4.
      ~- {1:`f`} uses {2:`r`} as `corrupted`#5 data.
        ~- {2:`r`}'s mutable field `contents` is being written.
      ~= A function that closes over `uncontended`#6 data is `nonportable`#7.
|}]
