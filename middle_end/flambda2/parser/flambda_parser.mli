
(* The type of tokens. *)

type token = 
  | TILDE
  | SYMBOL of (Fexpr.compilation_unit option * string)
  | STRING of (string)
  | STATIC_CONST_VALUE_ARRAY
  | STATIC_CONST_FLOAT_BLOCK
  | STATIC_CONST_FLOAT_ARRAY
  | STATIC_CONST_EMPTY_ARRAY
  | STATIC_CONST_BLOCK
  | STAR
  | SEMICOLON
  | RPAREN
  | RBRACKPIPE
  | RBRACK
  | RBRACE
  | PRIM of (string)
  | PIPE
  | MINUSGREATER
  | LPAREN
  | LBRACKPIPE
  | LBRACK
  | LBRACE
  | KWD_WITH
  | KWD_WHERE
  | KWD_VAL
  | KWD_UNROLL
  | KWD_UNREACHABLE
  | KWD_UNIT
  | KWD_TUPLED
  | KWD_TOPLEVEL
  | KWD_TAILREC
  | KWD_TAGGED
  | KWD_SWITCH
  | KWD_SUCC
  | KWD_SIZE
  | KWD_SET_OF_CLOSURES
  | KWD_RERAISE
  | KWD_REGULAR
  | KWD_REGION
  | KWD_REC_INFO
  | KWD_REC
  | KWD_PUSH
  | KWD_POP
  | KWD_OF
  | KWD_NULL
  | KWD_NOTRACE
  | KWD_NOALLOC
  | KWD_NEWER_VERSION_OF
  | KWD_NEVER
  | KWD_NATIVEINT
  | KWD_MUTABLE
  | KWD_LOOPIFY
  | KWD_LOCAL
  | KWD_LET
  | KWD_INVALID
  | KWD_INT64
  | KWD_INT32
  | KWD_INLINING_STATE
  | KWD_INLINED
  | KWD_INLINE
  | KWD_INF
  | KWD_IN
  | KWD_IMMUTABLE_UNIQUE
  | KWD_IMM
  | KWD_ID
  | KWD_HINT
  | KWD_HCF
  | KWD_FLOAT
  | KWD_EXN
  | KWD_ERROR
  | KWD_END
  | KWD_DO_NOT_INLINE
  | KWD_DONE
  | KWD_DIRECT
  | KWD_DEPTH
  | KWD_DELETED
  | KWD_DEFINE_ROOT_SYMBOL
  | KWD_DEFAULT
  | KWD_CONT
  | KWD_CODE
  | KWD_CLOSURE
  | KWD_CCALL
  | KWD_BOXED
  | KWD_AVAILABLE
  | KWD_ARRAY
  | KWD_APPLY
  | KWD_ANY
  | KWD_ANDWHERE
  | KWD_AND
  | KWD_ALWAYS
  | INT of (string * char option)
  | IDENT of (string)
  | FLOAT of (float)
  | EQUAL
  | EOF
  | DOT
  | COMMA
  | COLON
  | CARET
  | BLANK
  | BIGARROW
  | AT
  | AMP

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val flambda_unit: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Fexpr.flambda_unit)

val expect_test_spec: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Fexpr.expect_test_spec)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include CamlinternalMenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val flambda_unit: Lexing.position -> (Fexpr.flambda_unit) MenhirInterpreter.checkpoint
  
  val expect_test_spec: Lexing.position -> (Fexpr.expect_test_spec) MenhirInterpreter.checkpoint
  
end
