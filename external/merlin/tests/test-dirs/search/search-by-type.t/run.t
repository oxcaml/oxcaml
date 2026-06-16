  $ $MERLIN single search-by-type -filename ./context.ml \
  > -position 5:25 -limit 10 -query "string -> int option" |
  > tr '\n' ' ' | jq  '.value[] | {name,type,cost,doc}'
  {
    "name": "int_of_string_opt",
    "type": "string -> int option",
    "cost": 0,
    "doc": null
  }
  {
    "name": "int_of_string_opt",
    "type": "string -> int option",
    "cost": 0,
    "doc": null
  }
  {
    "name": "Int32.of_string_opt",
    "type": "string -> int32 option",
    "cost": 2,
    "doc": null
  }
  {
    "name": "Int64.of_string_opt",
    "type": "string -> int64 option",
    "cost": 2,
    "doc": null
  }
  {
    "name": "Sys.getenv_opt",
    "type": "string -> string option",
    "cost": 4,
    "doc": null
  }
  {
    "name": "bool_of_string_opt",
    "type": "string -> bool option",
    "cost": 4,
    "doc": null
  }
  {
    "name": "bool_of_string_opt",
    "type": "string -> bool option",
    "cost": 4,
    "doc": null
  }
  {
    "name": "Float.of_string_opt",
    "type": "string -> float option",
    "cost": 4,
    "doc": null
  }
  {
    "name": "float_of_string_opt",
    "type": "string -> float option",
    "cost": 4,
    "doc": null
  }
  {
    "name": "float_of_string_opt",
    "type": "string -> float option",
    "cost": 4,
    "doc": null
  }


  $ $MERLIN single search-by-type -filename ./context.ml \
  > -position 5:25 -limit 10 -query "('a -> 'b) -> 'a list -> 'b list" |
  > tr '\n' ' ' | jq  '.value[] | {name,type,cost,doc}'
  {
    "name": "List.map",
    "type": "('a -> 'b) -> 'a list -> 'b list",
    "cost": 0,
    "doc": null
  }
  {
    "name": "List.rev_map",
    "type": "('a -> 'b) -> 'a list -> 'b list",
    "cost": 0,
    "doc": null
  }
  {
    "name": "ListLabels.map",
    "type": "f:('a -> 'b) -> 'a list -> 'b list",
    "cost": 0,
    "doc": null
  }
  {
    "name": "ListLabels.rev_map",
    "type": "f:('a -> 'b) -> 'a list -> 'b list",
    "cost": 0,
    "doc": null
  }
  {
    "name": "List.mapi",
    "type": "(int -> 'a -> 'b) -> 'a list -> 'b list",
    "cost": 5,
    "doc": null
  }
  {
    "name": "ListLabels.mapi",
    "type": "f:(int -> 'a -> 'b) -> 'a list -> 'b list",
    "cost": 5,
    "doc": null
  }
  {
    "name": "List.concat_map",
    "type": "('a -> 'b list) -> 'a list -> 'b list",
    "cost": 10,
    "doc": null
  }
  {
    "name": "List.filter_map",
    "type": "('a -> 'b option) -> 'a list -> 'b list",
    "cost": 10,
    "doc": null
  }
  {
    "name": "ListLabels.concat_map",
    "type": "f:('a -> 'b list) -> 'a list -> 'b list",
    "cost": 10,
    "doc": null
  }
  {
    "name": "ListLabels.filter_map",
    "type": "f:('a -> 'b option) -> 'a list -> 'b list",
    "cost": 10,
    "doc": null
  }

  $ $MERLIN single search-by-type -filename ./context.ml \
  > -position 5:25 -limit 10 \
  > -query "Hashtbl : ('f, 'g) Hashtbl.t -> 'f -> 'g -> unit"
  {
    "class": "return",
    "value": [
      {
        "file": "hashtbl.mli",
        "start": {
          "line": 122,
          "col": 0
        },
        "end": {
          "line": 122,
          "col": 40
        },
        "name": "Hashtbl.add",
        "type": "('a, 'b) Hashtbl.t -> 'a -> 'b -> unit",
        "cost": 33,
        "doc": null,
        "constructible": "Hashtbl.add _ _ _"
      },
      {
        "file": "hashtbl.mli",
        "start": {
          "line": 157,
          "col": 0
        },
        "end": {
          "line": 157,
          "col": 44
        },
        "name": "Hashtbl.replace",
        "type": "('a, 'b) Hashtbl.t -> 'a -> 'b -> unit",
        "cost": 34,
        "doc": null,
        "constructible": "Hashtbl.replace _ _ _"
      },
      {
        "file": "hashtbl.mli",
        "start": {
          "line": 307,
          "col": 0
        },
        "end": {
          "line": 307,
          "col": 50
        },
        "name": "Hashtbl.add_seq",
        "type": "('a, 'b) Hashtbl.t -> ('a * 'b) Seq.t -> unit",
        "cost": 46,
        "doc": null,
        "constructible": "Hashtbl.add_seq _ _"
      },
      {
        "file": "hashtbl.mli",
        "start": {
          "line": 311,
          "col": 0
        },
        "end": {
          "line": 311,
          "col": 54
        },
        "name": "Hashtbl.replace_seq",
        "type": "('a, 'b) Hashtbl.t -> ('a * 'b) Seq.t -> unit",
        "cost": 47,
        "doc": null,
        "constructible": "Hashtbl.replace_seq _ _"
      },
      {
        "file": "moreLabels.mli",
        "start": {
          "line": 326,
          "col": 2
        },
        "end": {
          "line": 326,
          "col": 52
        },
        "name": "MoreLabels.Hashtbl.add_seq",
        "type": "('a, 'b) MoreLabels.Hashtbl.t -> ('a * 'b) Seq.t -> unit",
        "cost": 48,
        "doc": null,
        "constructible": "MoreLabels.Hashtbl.add_seq _ _"
      },
      {
        "file": "moreLabels.mli",
        "start": {
          "line": 330,
          "col": 2
        },
        "end": {
          "line": 330,
          "col": 56
        },
        "name": "MoreLabels.Hashtbl.replace_seq",
        "type": "('a, 'b) MoreLabels.Hashtbl.t -> ('a * 'b) Seq.t -> unit",
        "cost": 49,
        "doc": null,
        "constructible": "MoreLabels.Hashtbl.replace_seq _ _"
      },
      {
        "file": "result.mli",
        "start": {
          "line": 64,
          "col": 0
        },
        "end": {
          "line": 65,
          "col": 63
        },
        "name": "Result.bind",
        "type": "('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result",
        "cost": 61,
        "doc": null,
        "constructible": "Result.bind _ _"
      },
      {
        "file": "result.mli",
        "start": {
          "line": 76,
          "col": 0
        },
        "end": {
          "line": 77,
          "col": 60
        },
        "name": "Result.product",
        "type": "('a, 'e) result -> ('b, 'e) result -> ('a * 'b, 'e) result",
        "cost": 61,
        "doc": null,
        "constructible": "Result.product _ _"
      },
      {
        "file": "result.mli",
        "start": {
          "line": 158,
          "col": 2
        },
        "end": {
          "line": 158,
          "col": 75
        },
        "name": "Result.Syntax.and*",
        "type": "('a, 'e) result -> ('b, 'e) result -> ('a * 'b, 'e) result",
        "cost": 62,
        "doc": null,
        "constructible": "Result.Syntax.and* _ _"
      },
      {
        "file": "result.mli",
        "start": {
          "line": 164,
          "col": 2
        },
        "end": {
          "line": 164,
          "col": 75
        },
        "name": "Result.Syntax.and+",
        "type": "('a, 'e) result -> ('b, 'e) result -> ('a * 'b, 'e) result",
        "cost": 62,
        "doc": null,
        "constructible": "Result.Syntax.and+ _ _"
      }
    ],
    "notifications": []
  }


  $ $MERLIN single search-by-type -filename ./context.ml \
  > -position 5:25 -limit 10 -with-doc true -query "string -> int option" |
  > tr '\n' ' ' | jq  '.value[] | {name,type,cost,doc}'
  {
    "name": "int_of_string_opt",
    "type": "string -> int option",
    "cost": 0,
    "doc": "Convert the given string to an integer.    The string is read in decimal (by default, or if the string    begins with [0u]), in hexadecimal (if it begins with [0x] or    [0X]), in octal (if it begins with [0o] or [0O]), or in binary    (if it begins with [0b] or [0B]).     The [0u] prefix reads the input as an unsigned integer in the range    [[0, 2*max_int+1]].  If the input exceeds {!max_int}    it is converted to the signed integer    [min_int + input - max_int - 1].     The [_] (underscore) character can appear anywhere in the string    and is ignored.     Return [None] if the given string is not a valid representation of an    integer, or if the integer represented exceeds the range of integers    representable in type [int].    @since 4.05"
  }
  {
    "name": "int_of_string_opt",
    "type": "string -> int option",
    "cost": 0,
    "doc": "Convert the given string to an integer.    The string is read in decimal (by default, or if the string    begins with [0u]), in hexadecimal (if it begins with [0x] or    [0X]), in octal (if it begins with [0o] or [0O]), or in binary    (if it begins with [0b] or [0B]).     The [0u] prefix reads the input as an unsigned integer in the range    [[0, 2*max_int+1]].  If the input exceeds {!max_int}    it is converted to the signed integer    [min_int + input - max_int - 1].     The [_] (underscore) character can appear anywhere in the string    and is ignored.     Return [None] if the given string is not a valid representation of an    integer, or if the integer represented exceeds the range of integers    representable in type [int].    @since 4.05"
  }
  {
    "name": "Int32.of_string_opt",
    "type": "string -> int32 option",
    "cost": 2,
    "doc": "Same as [of_string], but return [None] instead of raising.     @since 4.05"
  }
  {
    "name": "Int64.of_string_opt",
    "type": "string -> int64 option",
    "cost": 2,
    "doc": "Same as [of_string], but return [None] instead of raising.     @since 4.05"
  }
  {
    "name": "Sys.getenv_opt",
    "type": "string -> string option",
    "cost": 4,
    "doc": "Return the value associated to a variable in the process     environment or [None] if the variable is unbound.     @since 4.05"
  }
  {
    "name": "bool_of_string_opt",
    "type": "string -> bool option",
    "cost": 4,
    "doc": "Convert the given string to a boolean.     Return [None] if the string is not [\"true\"] or [\"false\"].    @since 4.05"
  }
  {
    "name": "bool_of_string_opt",
    "type": "string -> bool option",
    "cost": 4,
    "doc": "Convert the given string to a boolean.     Return [None] if the string is not [\"true\"] or [\"false\"].    @since 4.05"
  }
  {
    "name": "Float.of_string_opt",
    "type": "string -> float option",
    "cost": 4,
    "doc": "Same as [of_string], but returns [None] instead of raising."
  }
  {
    "name": "float_of_string_opt",
    "type": "string -> float option",
    "cost": 4,
    "doc": "Convert the given string to a float.  The string is read in decimal    (by default) or in hexadecimal (marked by [0x] or [0X]).     The format of decimal floating-point numbers is    [ [-] dd.ddd (e|E) [+|-] dd ], where [d] stands for a decimal digit.     The format of hexadecimal floating-point numbers is    [ [-] 0(x|X) hh.hhh (p|P) [+|-] dd ], where [h] stands for an    hexadecimal digit and [d] for a decimal digit.     In both cases, at least one of the integer and fractional parts must be    given; the exponent part is optional.     The [_] (underscore) character can appear anywhere in the string    and is ignored.     Depending on the execution platforms, other representations of    floating-point numbers can be accepted, but should not be relied upon.     Return [None] if the given string is not a valid representation of a float.    @since 4.05"
  }
  {
    "name": "float_of_string_opt",
    "type": "string -> float option",
    "cost": 4,
    "doc": "Convert the given string to a float.  The string is read in decimal    (by default) or in hexadecimal (marked by [0x] or [0X]).     The format of decimal floating-point numbers is    [ [-] dd.ddd (e|E) [+|-] dd ], where [d] stands for a decimal digit.     The format of hexadecimal floating-point numbers is    [ [-] 0(x|X) hh.hhh (p|P) [+|-] dd ], where [h] stands for an    hexadecimal digit and [d] for a decimal digit.     In both cases, at least one of the integer and fractional parts must be    given; the exponent part is optional.     The [_] (underscore) character can appear anywhere in the string    and is ignored.     Depending on the execution platforms, other representations of    floating-point numbers can be accepted, but should not be relied upon.     Return [None] if the given string is not a valid representation of a float.    @since 4.05"
  }
