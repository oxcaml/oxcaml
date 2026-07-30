(* [Lexer.is_keyword] used by the copy of [Pprintast] linked into the standard
   library for runtime metaprogramming (to decide whether an identifier must be
   escaped). The real [Lexer] is generated from [lexer.mll] and pulls in the
   whole [Parser], so it cannot be copied; the keyword set from [lexer.mll]'s
   [all_keywords] is reproduced here. *)

let keywords =
  [ "and"; "as"; "assert"; "begin"; "borrow_"; "class"; "constraint"; "do";
    "done"; "downto"; "effect"; "else"; "end"; "exception"; "exclave_";
    "external"; "false"; "for"; "fun"; "function"; "functor"; "global_"; "if";
    "in"; "include"; "inherit"; "initializer"; "kind_"; "kind_of_"; "layout_";
    "lazy"; "let"; "local_"; "match"; "method"; "mod"; "module"; "mutable";
    "new"; "nonrec"; "object"; "of"; "open"; "or"; "overwrite_"; "poly_";
    "private"; "rec"; "repr_"; "sig"; "stack_"; "struct"; "then"; "to"; "true";
    "try"; "type"; "val"; "virtual"; "when"; "while"; "with"; "lor"; "lxor";
    "land"; "lsl"; "lsr"; "asr" ]

let table =
  lazy
    (let h = Hashtbl.create 149 in
     List.iter (fun k -> Hashtbl.replace h k ()) keywords;
     h)

let is_keyword name = Hashtbl.mem (Lazy.force table) name
