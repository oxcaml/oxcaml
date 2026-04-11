(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                   Jérémie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* BEGIN of BLACK MAGIC *)
(*$ open Ast_cinaps_helpers $*)

type _ witnesses = ..

type _ migration = ..
type _ migration += Undefined : _ migration

type 'a migration_info = {
  mutable next_version : 'a migration;
  mutable previous_version : 'a migration;
}

(** Abstract view of a version of an OCaml Ast *)
module type Ast = sig
  (*$ foreach_module (fun m types ->
      printf "  module %s : sig\n" m;
      List.iter types ~f:(printf "    type %s\n");
      printf "  end\n"
    )
  *)
  module Parsetree : sig
    type structure
    type signature
    type toplevel_phrase
    type core_type
    type expression
    type pattern
    type case
    type type_declaration
    type type_extension
    type extension_constructor
    type class_expr
    type class_field
    type class_type
    type class_signature
    type class_type_field
    type module_expr
    type module_type
    type signature_item
    type structure_item
  end
(*$*)
  module Config : sig
    val ast_impl_magic_number : string
    val ast_intf_magic_number : string
  end
end

(* Shortcuts for talking about ast types outside of the module language *)

type 'a _types = 'a constraint 'a
  = <
    (*$ foreach_type (fun _ s -> printf "    %-21s : _;\n" s) *)
    structure             : _;
    signature             : _;
    toplevel_phrase       : _;
    core_type             : _;
    expression            : _;
    pattern               : _;
    case                  : _;
    type_declaration      : _;
    type_extension        : _;
    extension_constructor : _;
    class_expr            : _;
    class_field           : _;
    class_type            : _;
    class_signature       : _;
    class_type_field      : _;
    module_expr           : _;
    module_type           : _;
    signature_item        : _;
    structure_item        : _;
(*$*)
  >
;;

(*$ foreach_type (fun _ s ->
    printf "type 'a get_%s =\n" s;
    printf "  'x constraint 'a _types = < %s : 'x; .. >\n" s
  ) *)
type 'a get_structure =
  'x constraint 'a _types = < structure : 'x; .. >
type 'a get_signature =
  'x constraint 'a _types = < signature : 'x; .. >
type 'a get_toplevel_phrase =
  'x constraint 'a _types = < toplevel_phrase : 'x; .. >
type 'a get_core_type =
  'x constraint 'a _types = < core_type : 'x; .. >
type 'a get_expression =
  'x constraint 'a _types = < expression : 'x; .. >
type 'a get_pattern =
  'x constraint 'a _types = < pattern : 'x; .. >
type 'a get_case =
  'x constraint 'a _types = < case : 'x; .. >
type 'a get_type_declaration =
  'x constraint 'a _types = < type_declaration : 'x; .. >
type 'a get_type_extension =
  'x constraint 'a _types = < type_extension : 'x; .. >
type 'a get_extension_constructor =
  'x constraint 'a _types = < extension_constructor : 'x; .. >
type 'a get_class_expr =
  'x constraint 'a _types = < class_expr : 'x; .. >
type 'a get_class_field =
  'x constraint 'a _types = < class_field : 'x; .. >
type 'a get_class_type =
  'x constraint 'a _types = < class_type : 'x; .. >
type 'a get_class_signature =
  'x constraint 'a _types = < class_signature : 'x; .. >
type 'a get_class_type_field =
  'x constraint 'a _types = < class_type_field : 'x; .. >
type 'a get_module_expr =
  'x constraint 'a _types = < module_expr : 'x; .. >
type 'a get_module_type =
  'x constraint 'a _types = < module_type : 'x; .. >
type 'a get_signature_item =
  'x constraint 'a _types = < signature_item : 'x; .. >
type 'a get_structure_item =
  'x constraint 'a _types = < structure_item : 'x; .. >
(*$*)

module type OCaml_version = sig
  module Ast : Ast
  val version : int
  val string_version : string
  type types = <
    (*$ foreach_type (fun m s -> printf "    %-21s : Ast.%s.%s;\n" s m s)*)
    structure             : Ast.Parsetree.structure;
    signature             : Ast.Parsetree.signature;
    toplevel_phrase       : Ast.Parsetree.toplevel_phrase;
    core_type             : Ast.Parsetree.core_type;
    expression            : Ast.Parsetree.expression;
    pattern               : Ast.Parsetree.pattern;
    case                  : Ast.Parsetree.case;
    type_declaration      : Ast.Parsetree.type_declaration;
    type_extension        : Ast.Parsetree.type_extension;
    extension_constructor : Ast.Parsetree.extension_constructor;
    class_expr            : Ast.Parsetree.class_expr;
    class_field           : Ast.Parsetree.class_field;
    class_type            : Ast.Parsetree.class_type;
    class_signature       : Ast.Parsetree.class_signature;
    class_type_field      : Ast.Parsetree.class_type_field;
    module_expr           : Ast.Parsetree.module_expr;
    module_type           : Ast.Parsetree.module_type;
    signature_item        : Ast.Parsetree.signature_item;
    structure_item        : Ast.Parsetree.structure_item;
(*$*)
  > _types
  type _ witnesses += Version : types witnesses
  val migration_info : types migration_info
end

module Make_witness(Ast : Ast) =
struct
  type types = <
    (*$ foreach_type (fun m s -> printf "    %-21s : Ast.%s.%s;\n" s m s)*)
    structure             : Ast.Parsetree.structure;
    signature             : Ast.Parsetree.signature;
    toplevel_phrase       : Ast.Parsetree.toplevel_phrase;
    core_type             : Ast.Parsetree.core_type;
    expression            : Ast.Parsetree.expression;
    pattern               : Ast.Parsetree.pattern;
    case                  : Ast.Parsetree.case;
    type_declaration      : Ast.Parsetree.type_declaration;
    type_extension        : Ast.Parsetree.type_extension;
    extension_constructor : Ast.Parsetree.extension_constructor;
    class_expr            : Ast.Parsetree.class_expr;
    class_field           : Ast.Parsetree.class_field;
    class_type            : Ast.Parsetree.class_type;
    class_signature       : Ast.Parsetree.class_signature;
    class_type_field      : Ast.Parsetree.class_type_field;
    module_expr           : Ast.Parsetree.module_expr;
    module_type           : Ast.Parsetree.module_type;
    signature_item        : Ast.Parsetree.signature_item;
    structure_item        : Ast.Parsetree.structure_item;
(*$*)
  > _types
  type _ witnesses += Version : types witnesses
  let migration_info : types migration_info =
    { next_version = Undefined; previous_version = Undefined }
end

type 'types ocaml_version =
  (module OCaml_version
    (*$ let sep = with_then_and () in
      foreach_type (fun m s ->
          printf "    %t type Ast.%s.%s = 'types get_%s\n" sep m s s) *)
    with type Ast.Parsetree.structure = 'types get_structure
     and type Ast.Parsetree.signature = 'types get_signature
     and type Ast.Parsetree.toplevel_phrase = 'types get_toplevel_phrase
     and type Ast.Parsetree.core_type = 'types get_core_type
     and type Ast.Parsetree.expression = 'types get_expression
     and type Ast.Parsetree.pattern = 'types get_pattern
     and type Ast.Parsetree.case = 'types get_case
     and type Ast.Parsetree.type_declaration = 'types get_type_declaration
     and type Ast.Parsetree.type_extension = 'types get_type_extension
     and type Ast.Parsetree.extension_constructor = 'types get_extension_constructor
     and type Ast.Parsetree.class_expr = 'types get_class_expr
     and type Ast.Parsetree.class_field = 'types get_class_field
     and type Ast.Parsetree.class_type = 'types get_class_type
     and type Ast.Parsetree.class_signature = 'types get_class_signature
     and type Ast.Parsetree.class_type_field = 'types get_class_type_field
     and type Ast.Parsetree.module_expr = 'types get_module_expr
     and type Ast.Parsetree.module_type = 'types get_module_type
     and type Ast.Parsetree.signature_item = 'types get_signature_item
     and type Ast.Parsetree.structure_item = 'types get_structure_item
(*$*)
  )

type ('from, 'to_) migration_functions = {
  (*$ foreach_type (fun _ s ->
      printf "  copy_%s: 'from get_%s -> 'to_ get_%s;\n" s s s) *)
  copy_structure: 'from get_structure -> 'to_ get_structure;
  copy_signature: 'from get_signature -> 'to_ get_signature;
  copy_toplevel_phrase: 'from get_toplevel_phrase -> 'to_ get_toplevel_phrase;
  copy_core_type: 'from get_core_type -> 'to_ get_core_type;
  copy_expression: 'from get_expression -> 'to_ get_expression;
  copy_pattern: 'from get_pattern -> 'to_ get_pattern;
  copy_case: 'from get_case -> 'to_ get_case;
  copy_type_declaration: 'from get_type_declaration -> 'to_ get_type_declaration;
  copy_type_extension: 'from get_type_extension -> 'to_ get_type_extension;
  copy_extension_constructor: 'from get_extension_constructor -> 'to_ get_extension_constructor;
  copy_class_expr: 'from get_class_expr -> 'to_ get_class_expr;
  copy_class_field: 'from get_class_field -> 'to_ get_class_field;
  copy_class_type: 'from get_class_type -> 'to_ get_class_type;
  copy_class_signature: 'from get_class_signature -> 'to_ get_class_signature;
  copy_class_type_field: 'from get_class_type_field -> 'to_ get_class_type_field;
  copy_module_expr: 'from get_module_expr -> 'to_ get_module_expr;
  copy_module_type: 'from get_module_type -> 'to_ get_module_type;
  copy_signature_item: 'from get_signature_item -> 'to_ get_signature_item;
  copy_structure_item: 'from get_structure_item -> 'to_ get_structure_item;
(*$*)
}

let id x = x
let migration_identity : ('a, 'a) migration_functions = {
  (*$ foreach_type (fun _ s -> printf "  copy_%s = id;\n" s) *)
  copy_structure = id;
  copy_signature = id;
  copy_toplevel_phrase = id;
  copy_core_type = id;
  copy_expression = id;
  copy_pattern = id;
  copy_case = id;
  copy_type_declaration = id;
  copy_type_extension = id;
  copy_extension_constructor = id;
  copy_class_expr = id;
  copy_class_field = id;
  copy_class_type = id;
  copy_class_signature = id;
  copy_class_type_field = id;
  copy_module_expr = id;
  copy_module_type = id;
  copy_signature_item = id;
  copy_structure_item = id;
(*$*)
}

let compose f g x = f (g x)
let migration_compose (ab : ('a, 'b) migration_functions) (bc : ('b, 'c) migration_functions) : ('a, 'c) migration_functions = {
  (*$ foreach_type (fun _ s ->
      printf "  copy_%-21s = compose bc.copy_%-21s ab.copy_%s;\n" s s s) *)
  copy_structure             = compose bc.copy_structure             ab.copy_structure;
  copy_signature             = compose bc.copy_signature             ab.copy_signature;
  copy_toplevel_phrase       = compose bc.copy_toplevel_phrase       ab.copy_toplevel_phrase;
  copy_core_type             = compose bc.copy_core_type             ab.copy_core_type;
  copy_expression            = compose bc.copy_expression            ab.copy_expression;
  copy_pattern               = compose bc.copy_pattern               ab.copy_pattern;
  copy_case                  = compose bc.copy_case                  ab.copy_case;
  copy_type_declaration      = compose bc.copy_type_declaration      ab.copy_type_declaration;
  copy_type_extension        = compose bc.copy_type_extension        ab.copy_type_extension;
  copy_extension_constructor = compose bc.copy_extension_constructor ab.copy_extension_constructor;
  copy_class_expr            = compose bc.copy_class_expr            ab.copy_class_expr;
  copy_class_field           = compose bc.copy_class_field           ab.copy_class_field;
  copy_class_type            = compose bc.copy_class_type            ab.copy_class_type;
  copy_class_signature       = compose bc.copy_class_signature       ab.copy_class_signature;
  copy_class_type_field      = compose bc.copy_class_type_field      ab.copy_class_type_field;
  copy_module_expr           = compose bc.copy_module_expr           ab.copy_module_expr;
  copy_module_type           = compose bc.copy_module_type           ab.copy_module_type;
  copy_signature_item        = compose bc.copy_signature_item        ab.copy_signature_item;
  copy_structure_item        = compose bc.copy_structure_item        ab.copy_structure_item;
(*$*)
}

type _ migration += Migration : 'from ocaml_version * ('from, 'to_) migration_functions * 'to_ ocaml_version -> 'from migration

module type Migrate_module = sig
  module From : Ast
  module To : Ast
  (*$ foreach_type (fun m s ->
      printf "  val copy_%-21s: From.%s.%s -> To.%s.%s\n" s m s m s) *)
  val copy_structure            : From.Parsetree.structure -> To.Parsetree.structure
  val copy_signature            : From.Parsetree.signature -> To.Parsetree.signature
  val copy_toplevel_phrase      : From.Parsetree.toplevel_phrase -> To.Parsetree.toplevel_phrase
  val copy_core_type            : From.Parsetree.core_type -> To.Parsetree.core_type
  val copy_expression           : From.Parsetree.expression -> To.Parsetree.expression
  val copy_pattern              : From.Parsetree.pattern -> To.Parsetree.pattern
  val copy_case                 : From.Parsetree.case -> To.Parsetree.case
  val copy_type_declaration     : From.Parsetree.type_declaration -> To.Parsetree.type_declaration
  val copy_type_extension       : From.Parsetree.type_extension -> To.Parsetree.type_extension
  val copy_extension_constructor: From.Parsetree.extension_constructor -> To.Parsetree.extension_constructor
  val copy_class_expr           : From.Parsetree.class_expr -> To.Parsetree.class_expr
  val copy_class_field          : From.Parsetree.class_field -> To.Parsetree.class_field
  val copy_class_type           : From.Parsetree.class_type -> To.Parsetree.class_type
  val copy_class_signature      : From.Parsetree.class_signature -> To.Parsetree.class_signature
  val copy_class_type_field     : From.Parsetree.class_type_field -> To.Parsetree.class_type_field
  val copy_module_expr          : From.Parsetree.module_expr -> To.Parsetree.module_expr
  val copy_module_type          : From.Parsetree.module_type -> To.Parsetree.module_type
  val copy_signature_item       : From.Parsetree.signature_item -> To.Parsetree.signature_item
  val copy_structure_item       : From.Parsetree.structure_item -> To.Parsetree.structure_item
(*$*)
end

module Migration_functions
    (A : OCaml_version) (B : OCaml_version)
    (A_to_B : Migrate_module with module From = A.Ast and module To = B.Ast)
=
struct
  let migration_functions : (A.types, B.types) migration_functions =
    let open A_to_B in
    {
      (*$ foreach_type (fun _ s -> printf "      copy_%s;\n" s) *)
      copy_structure;
      copy_signature;
      copy_toplevel_phrase;
      copy_core_type;
      copy_expression;
      copy_pattern;
      copy_case;
      copy_type_declaration;
      copy_type_extension;
      copy_extension_constructor;
      copy_class_expr;
      copy_class_field;
      copy_class_type;
      copy_class_signature;
      copy_class_type_field;
      copy_module_expr;
      copy_module_type;
      copy_signature_item;
      copy_structure_item;
(*$*)
    }
end

module Register_migration (A : OCaml_version) (B : OCaml_version)
    (A_to_B : Migrate_module with module From = A.Ast and module To = B.Ast)
    (B_to_A : Migrate_module with module From = B.Ast and module To = A.Ast)
=
struct
  let () = (
    let is_undefined : type a. a migration -> bool = function
      | Undefined -> true
      | _ -> false
    in
    assert (A.version < B.version);
    assert (is_undefined A.migration_info.next_version);
    assert (is_undefined B.migration_info.previous_version);
    let module A_to_B_fun = Migration_functions(A)(B)(A_to_B) in
    let module B_to_A_fun = Migration_functions(B)(A)(B_to_A) in
    A.migration_info.next_version <-
      Migration ((module A), A_to_B_fun.migration_functions, (module B));
    B.migration_info.previous_version <-
      Migration ((module B), B_to_A_fun.migration_functions, (module A));
  )
end

type 'from immediate_migration =
  | No_migration : 'from immediate_migration
  | Immediate_migration
    :  ('from, 'to_) migration_functions * 'to_ ocaml_version
      -> 'from immediate_migration

let immediate_migration
    (*$ foreach_type (fun _ s -> printf "    (type %s)\n" s) *)
    (type structure)
    (type signature)
    (type toplevel_phrase)
    (type core_type)
    (type expression)
    (type pattern)
    (type case)
    (type type_declaration)
    (type type_extension)
    (type extension_constructor)
    (type class_expr)
    (type class_field)
    (type class_type)
    (type class_signature)
    (type class_type_field)
    (type module_expr)
    (type module_type)
    (type signature_item)
    (type structure_item)
(*$*)
    ((module A) : <
     (*$ foreach_type (fun _ s -> printf  "     %-21s : %s;\n" s s) *)
     structure             : structure;
     signature             : signature;
     toplevel_phrase       : toplevel_phrase;
     core_type             : core_type;
     expression            : expression;
     pattern               : pattern;
     case                  : case;
     type_declaration      : type_declaration;
     type_extension        : type_extension;
     extension_constructor : extension_constructor;
     class_expr            : class_expr;
     class_field           : class_field;
     class_type            : class_type;
     class_signature       : class_signature;
     class_type_field      : class_type_field;
     module_expr           : module_expr;
     module_type           : module_type;
     signature_item        : signature_item;
     structure_item        : structure_item;
(*$*)
     > ocaml_version)
    direction
  =
  let version = match direction with
    | `Next -> A.migration_info.next_version
    | `Previous -> A.migration_info.previous_version
  in
  match version with
  | Undefined -> No_migration
  | Migration (_, funs, to_) -> Immediate_migration (funs, to_)
  | _ -> assert false

let migrate
    (*$ foreach_type (fun _ s -> printf "    (type %s1) (type %s2)\n" s s) *)
    (type structure1) (type structure2)
    (type signature1) (type signature2)
    (type toplevel_phrase1) (type toplevel_phrase2)
    (type core_type1) (type core_type2)
    (type expression1) (type expression2)
    (type pattern1) (type pattern2)
    (type case1) (type case2)
    (type type_declaration1) (type type_declaration2)
    (type type_extension1) (type type_extension2)
    (type extension_constructor1) (type extension_constructor2)
    (type class_expr1) (type class_expr2)
    (type class_field1) (type class_field2)
    (type class_type1) (type class_type2)
    (type class_signature1) (type class_signature2)
    (type class_type_field1) (type class_type_field2)
    (type module_expr1) (type module_expr2)
    (type module_type1) (type module_type2)
    (type signature_item1) (type signature_item2)
    (type structure_item1) (type structure_item2)
(*$*)
    ((module A) : <
     (*$ foreach_type (fun _ s -> printf "     %-21s : %s1;\n" s s) *)
     structure             : structure1;
     signature             : signature1;
     toplevel_phrase       : toplevel_phrase1;
     core_type             : core_type1;
     expression            : expression1;
     pattern               : pattern1;
     case                  : case1;
     type_declaration      : type_declaration1;
     type_extension        : type_extension1;
     extension_constructor : extension_constructor1;
     class_expr            : class_expr1;
     class_field           : class_field1;
     class_type            : class_type1;
     class_signature       : class_signature1;
     class_type_field      : class_type_field1;
     module_expr           : module_expr1;
     module_type           : module_type1;
     signature_item        : signature_item1;
     structure_item        : structure_item1;
(*$*)
     > ocaml_version)
    ((module B) : <
     (*$ foreach_type (fun _ s -> printf "     %-21s : %s2;\n" s s) *)
     structure             : structure2;
     signature             : signature2;
     toplevel_phrase       : toplevel_phrase2;
     core_type             : core_type2;
     expression            : expression2;
     pattern               : pattern2;
     case                  : case2;
     type_declaration      : type_declaration2;
     type_extension        : type_extension2;
     extension_constructor : extension_constructor2;
     class_expr            : class_expr2;
     class_field           : class_field2;
     class_type            : class_type2;
     class_signature       : class_signature2;
     class_type_field      : class_type_field2;
     module_expr           : module_expr2;
     module_type           : module_type2;
     signature_item        : signature_item2;
     structure_item        : structure_item2;
(*$*)
     > ocaml_version)
  : (A.types, B.types) migration_functions
  =
  match A.Version with
  | B.Version -> migration_identity
  | _ ->
    let direction = if A.version < B.version then `Next else `Previous in
    let rec migrate (m : A.types immediate_migration) : (A.types, B.types) migration_functions =
      match m with
      | No_migration -> assert false
      | Immediate_migration (f, (module To)) ->
        match To.Version with
        | B.Version -> f
        | _ ->
          match immediate_migration (module To) direction with
          | No_migration -> assert false
          | Immediate_migration (g, to2) ->
            migrate (Immediate_migration (migration_compose f g, to2))
    in
    migrate (immediate_migration (module A) direction)

module Convert (A : OCaml_version) (B : OCaml_version) = struct
  let {
    (*$ foreach_type (fun _ s -> printf "    copy_%s;\n" s) *)
    copy_structure;
    copy_signature;
    copy_toplevel_phrase;
    copy_core_type;
    copy_expression;
    copy_pattern;
    copy_case;
    copy_type_declaration;
    copy_type_extension;
    copy_extension_constructor;
    copy_class_expr;
    copy_class_field;
    copy_class_type;
    copy_class_signature;
    copy_class_type_field;
    copy_module_expr;
    copy_module_type;
    copy_signature_item;
    copy_structure_item;
(*$*)
  } : (A.types, B.types) migration_functions =
    migrate (module A) (module B)
end

(*$ foreach_version (fun n version ->
    printf "module OCaml_%d = struct\n" n;
    printf "  module Ast = Astlib.Ast_%d\n" n;
    printf "  include Make_witness(Astlib.Ast_%d)\n" n;
    printf "  let version = %d\n" n;
    printf "  let string_version = %S\n" version;
    printf "end\n";
    printf "let ocaml_%d : OCaml_%d.types ocaml_version = (module OCaml_%d)\n"
      n n n;
  )
*)
module OCaml_408 = struct
  module Ast = Astlib.Ast_408
  include Make_witness(Astlib.Ast_408)
  let version = 408
  let string_version = "4.08"
end
let ocaml_408 : OCaml_408.types ocaml_version = (module OCaml_408)
module OCaml_409 = struct
  module Ast = Astlib.Ast_409
  include Make_witness(Astlib.Ast_409)
  let version = 409
  let string_version = "4.09"
end
let ocaml_409 : OCaml_409.types ocaml_version = (module OCaml_409)
module OCaml_410 = struct
  module Ast = Astlib.Ast_410
  include Make_witness(Astlib.Ast_410)
  let version = 410
  let string_version = "4.10"
end
let ocaml_410 : OCaml_410.types ocaml_version = (module OCaml_410)
module OCaml_411 = struct
  module Ast = Astlib.Ast_411
  include Make_witness(Astlib.Ast_411)
  let version = 411
  let string_version = "4.11"
end
let ocaml_411 : OCaml_411.types ocaml_version = (module OCaml_411)
module OCaml_412 = struct
  module Ast = Astlib.Ast_412
  include Make_witness(Astlib.Ast_412)
  let version = 412
  let string_version = "4.12"
end
let ocaml_412 : OCaml_412.types ocaml_version = (module OCaml_412)
module OCaml_413 = struct
  module Ast = Astlib.Ast_413
  include Make_witness(Astlib.Ast_413)
  let version = 413
  let string_version = "4.13"
end
let ocaml_413 : OCaml_413.types ocaml_version = (module OCaml_413)
module OCaml_414 = struct
  module Ast = Astlib.Ast_414
  include Make_witness(Astlib.Ast_414)
  let version = 414
  let string_version = "4.14"
end
let ocaml_414 : OCaml_414.types ocaml_version = (module OCaml_414)
module OCaml_500 = struct
  module Ast = Astlib.Ast_500
  include Make_witness(Astlib.Ast_500)
  let version = 500
  let string_version = "5.0"
end
let ocaml_500 : OCaml_500.types ocaml_version = (module OCaml_500)
module OCaml_501 = struct
  module Ast = Astlib.Ast_501
  include Make_witness(Astlib.Ast_501)
  let version = 501
  let string_version = "5.1"
end
let ocaml_501 : OCaml_501.types ocaml_version = (module OCaml_501)
module OCaml_502 = struct
  module Ast = Astlib.Ast_502
  include Make_witness(Astlib.Ast_502)
  let version = 502
  let string_version = "5.2"
end
let ocaml_502 : OCaml_502.types ocaml_version = (module OCaml_502)
module OCaml_503 = struct
  module Ast = Astlib.Ast_503
  include Make_witness(Astlib.Ast_503)
  let version = 503
  let string_version = "5.3"
end
let ocaml_503 : OCaml_503.types ocaml_version = (module OCaml_503)
module OCaml_504 = struct
  module Ast = Astlib.Ast_504
  include Make_witness(Astlib.Ast_504)
  let version = 504
  let string_version = "5.4"
end
let ocaml_504 : OCaml_504.types ocaml_version = (module OCaml_504)
module OCaml_505 = struct
  module Ast = Astlib.Ast_505
  include Make_witness(Astlib.Ast_505)
  let version = 505
  let string_version = "5.5"
end
let ocaml_505 : OCaml_505.types ocaml_version = (module OCaml_505)
(*$*)

let all_versions : (module OCaml_version) list = [
  (*$foreach_version (fun n _ ->
      printf "(module OCaml_%d : OCaml_version);\n" n)*)
(module OCaml_408 : OCaml_version);
(module OCaml_409 : OCaml_version);
(module OCaml_410 : OCaml_version);
(module OCaml_411 : OCaml_version);
(module OCaml_412 : OCaml_version);
(module OCaml_413 : OCaml_version);
(module OCaml_414 : OCaml_version);
(module OCaml_500 : OCaml_version);
(module OCaml_501 : OCaml_version);
(module OCaml_502 : OCaml_version);
(module OCaml_503 : OCaml_version);
(module OCaml_504 : OCaml_version);
(module OCaml_505 : OCaml_version);
(*$*)
]

(*$foreach_version_pair (fun a b ->
    printf "include Register_migration(OCaml_%d)(OCaml_%d)\n" a b;
    printf "    (Astlib.Migrate_%d_%d)(Astlib.Migrate_%d_%d)\n" a b b a
  )
*)
include Register_migration(OCaml_408)(OCaml_409)
    (Astlib.Migrate_408_409)(Astlib.Migrate_409_408)
include Register_migration(OCaml_409)(OCaml_410)
    (Astlib.Migrate_409_410)(Astlib.Migrate_410_409)
include Register_migration(OCaml_410)(OCaml_411)
    (Astlib.Migrate_410_411)(Astlib.Migrate_411_410)
include Register_migration(OCaml_411)(OCaml_412)
    (Astlib.Migrate_411_412)(Astlib.Migrate_412_411)
include Register_migration(OCaml_412)(OCaml_413)
    (Astlib.Migrate_412_413)(Astlib.Migrate_413_412)
include Register_migration(OCaml_413)(OCaml_414)
    (Astlib.Migrate_413_414)(Astlib.Migrate_414_413)
include Register_migration(OCaml_414)(OCaml_500)
    (Astlib.Migrate_414_500)(Astlib.Migrate_500_414)
include Register_migration(OCaml_500)(OCaml_501)
    (Astlib.Migrate_500_501)(Astlib.Migrate_501_500)
include Register_migration(OCaml_501)(OCaml_502)
    (Astlib.Migrate_501_502)(Astlib.Migrate_502_501)
include Register_migration(OCaml_502)(OCaml_503)
    (Astlib.Migrate_502_503)(Astlib.Migrate_503_502)
include Register_migration(OCaml_503)(OCaml_504)
    (Astlib.Migrate_503_504)(Astlib.Migrate_504_503)
include Register_migration(OCaml_504)(OCaml_505)
    (Astlib.Migrate_504_505)(Astlib.Migrate_505_504)
(*$*)

module OCaml_current = OCaml_OCAML_VERSION

module Find_version = struct
  type t = Impl of (module OCaml_version) | Intf of (module OCaml_version) | Unknown

  let from_magic magic =
    let rec loop = function
      | [] -> Unknown
      | (module Version : OCaml_version) :: tail ->
          if Version.Ast.Config.ast_impl_magic_number = magic then
            Impl (module Version)
          else if Version.Ast.Config.ast_intf_magic_number = magic then
            Intf (module Version)
          else
            loop tail
    in
    (* Traverse the versions from last to first:
       if the magic numbers aren't unique among versions,
       we want the latest version with a magic number match.
       The situation in mind is trunk support. *)
      let all_versions_top_down = List.rev all_versions in
      loop all_versions_top_down
end
