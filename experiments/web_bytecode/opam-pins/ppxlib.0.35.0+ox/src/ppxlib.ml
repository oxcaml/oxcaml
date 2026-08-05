(** Standard library for ppx rewriters *)

(** [ppxlib] is meant to be opened globally in your PPX source files.

    Opening it comes with two advantages. First, it will shadow the
    [compiler-libs] modules. The [compiler-libs] modules are unstable and aren't
    meant to be used, so shadowing them is a good protection mechanism. In case
    you don't want to open [Ppxlib], you can open [Ocaml_shadow] to get the same
    protection. Second, it will bring several modules in scope, that are useful
    to have when writing a rewriter:

    - The main [ppxlib] modules, such as modules to help manipulate the AST
      ({!Ast_builder}, {!Ast_pattern}), and a few functions.
    - Modules from other libraries, such as {!Ast_helper} or {!Pprintast},
    - The whole AST types (by [including] the {!Ast} module).

    {1 The core [ppxlib] entries} *)

(** {2 Manipulating the AST} *)

module Ast_builder = Ast_builder
module Ast_pattern = Ast_pattern
module Ast_traverse = Ast_traverse

(** {2 Context-free rewriting} *)

module Context_free = Context_free
module Deriving = Deriving
module Extension = Extension
module Expansion_context = Expansion_context
module Code_path = Code_path

(** {2 Other helpers} *)

module Expansion_helpers = Expansion_helpers
module Merlin_helpers = Merlin_helpers
module Spellcheck = Spellcheck
module Keyword = Keyword
module Pp_ast = Pp_ast

(** {2 Driver-related modules} *)

module Driver = Driver
module Caller_id = Caller_id
module Ast_io = Utils.Ast_io.Read_bin

(** {2 Checks} *)

module Attribute = Attribute
module Reserved_namespaces = Name.Reserved_namespaces

(** {2 Common helper functions} *)

include Common

(** {1 Modules from other libraries}

    Expose some modules from {!Ppxlib_ast}. *)

module Ast = Ppxlib_ast.Ast
module Ast_helper = Ppxlib_ast.Ast_helper
module Asttypes = Ppxlib_ast.Asttypes
module Parse = Ppxlib_ast.Parse
module Parsetree = Ppxlib_ast.Parsetree
module Pprintast = Ppxlib_ast.Pprintast
module Selected_ast = Ppxlib_ast.Selected_ast
module Location = Location
module Longident = Longident
module Loc = Loc

(** {1 The whole AST types} *)

include Ast
(** Include all the Ast definitions since we need them in every single ppx

    @closed *)

(** Make sure code using Ppxlib doesn't refer to compiler-libs without being
    explicit about it:

    @closed *)
include struct
  [@@@warning "-3"]

  open Ocaml_shadow

  include (
    Ocaml_shadow :
      module type of struct
        include Ocaml_shadow
      end
      with module Ast_helper := Ast_helper
      with module Asttypes := Asttypes
      with module Docstrings := Docstrings
      with module Identifiable := Identifiable
      with module Lexer := Lexer
      with module Location := Location
      with module Longident := Longident
      with module Parse := Parse
      with module Parsetree := Parsetree
      with module Pprintast := Pprintast
      with module Syntaxerr := Syntaxerr)
end

(**/**)

(* For tests, Ppx_core compatibility layer and use by ppxlib-pp-ast
   and other internal tools. *)
module Ppxlib_private = struct
  module Common = Common
  module Name = Name
  module Utils = Utils
end
