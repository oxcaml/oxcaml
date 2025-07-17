(* TEST
 expect;
*)

(* Array type arguments are [any mod separable]: *)
type t_any : any

type should_fail = t_any array

[%%expect{|
type t_any : any
Line 3, characters 19-24:
3 | type should_fail = t_any array
                       ^^^^^
Error: This type "t_any" should be an instance of type "('a : any mod separable)"
       The kind of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the kind of t_any must be a subkind of any mod separable
         because it's the type argument to the array type.
|}]

type t_value_or_null : value_or_null

type should_fail = t_value_or_null array

[%%expect{|
type t_value_or_null : value_or_null
Line 3, characters 19-34:
3 | type should_fail = t_value_or_null array
                       ^^^^^^^^^^^^^^^
Error: This type "t_value_or_null" should be an instance of type
         "('a : any mod separable)"
       The kind of t_value_or_null is value_or_null
         because of the definition of t_value_or_null at line 1, characters 0-36.
       But the kind of t_value_or_null must be a subkind of any mod separable
         because it's the type argument to the array type.
|}]

type t_any_mod_separable : any mod separable

type should_work = t_any_mod_separable array

[%%expect{|
type t_any_mod_separable : any mod separable
type should_work = t_any_mod_separable array
|}]

type t_value : value

type should_work = t_value array

[%%expect{|
type t_value
type should_work = t_value array
|}]

(* Concrete types with [or_null] *)

type should_work = int or_null array

[%%expect{|
type should_work = int or_null array
|}]

type should_work = string or_null array

[%%expect{|
type should_work = string or_null array
|}]

type should_fail = float or_null array

[%%expect{|
Line 1, characters 19-32:
1 | type should_fail = float or_null array
                       ^^^^^^^^^^^^^
Error: This type "float or_null" should be an instance of type
         "('a : any mod separable)"
       The kind of float or_null is
           value_or_null mod many unyielding stateless immutable
         because it is the primitive type or_null.
       But the kind of float or_null must be a subkind of any mod separable
         because it's the type argument to the array type.
|}, Principal{|
Line 1, characters 19-32:
1 | type should_fail = float or_null array
                       ^^^^^^^^^^^^^
Error: This type "float or_null" should be an instance of type
         "('a : any mod separable)"
       The kind of float or_null is value_or_null mod everything with float
         because it is the primitive type or_null.
       But the kind of float or_null must be a subkind of any mod separable
         because it's the type argument to the array type.
|}]

(* Test constructing arrays with [or_null] elements *)

let should_fail = [| Null; This 3.4 |]

[%%expect{|
Line 1, characters 32-35:
1 | let should_fail = [| Null; This 3.4 |]
                                    ^^^
Error: This expression has type "float" but an expression was expected of type
         "('a : value mod non_float)"
       The kind of float is value mod many unyielding stateless immutable
         because it is the primitive type float.
       But the kind of float must be a subkind of value mod non_float
         because it's the type of an array element.
|}]

type t_any : any

type should_fail = t_any iarray

[%%expect{|
type t_any : any
Line 3, characters 19-24:
3 | type should_fail = t_any iarray
                       ^^^^^
Error: This type "t_any" should be an instance of type "('a : any mod separable)"
       The kind of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the kind of t_any must be a subkind of any mod separable
         because it's the type argument to the array type.
|}]

type t_value_or_null : value_or_null

type should_fail = t_value_or_null iarray

[%%expect{|
type t_value_or_null : value_or_null
Line 3, characters 19-34:
3 | type should_fail = t_value_or_null iarray
                       ^^^^^^^^^^^^^^^
Error: This type "t_value_or_null" should be an instance of type
         "('a : any mod separable)"
       The kind of t_value_or_null is value_or_null
         because of the definition of t_value_or_null at line 1, characters 0-36.
       But the kind of t_value_or_null must be a subkind of any mod separable
         because it's the type argument to the array type.
|}]

type t_any_mod_separable : any mod separable

type should_work = t_any_mod_separable iarray

[%%expect{|
type t_any_mod_separable : any mod separable
type should_work = t_any_mod_separable iarray
|}]

type t_value : value

type should_work = t_value iarray

[%%expect{|
type t_value
type should_work = t_value iarray
|}]

(* Concrete types with [or_null] for [iarray]s *)

type should_work = int or_null iarray

[%%expect{|
type should_work = int or_null iarray
|}]

type should_work = string or_null iarray

[%%expect{|
type should_work = string or_null iarray
|}]

type should_fail = float or_null iarray

[%%expect{|
Line 1, characters 19-32:
1 | type should_fail = float or_null iarray
                       ^^^^^^^^^^^^^
Error: This type "float or_null" should be an instance of type
         "('a : any mod separable)"
       The kind of float or_null is
           value_or_null mod many unyielding stateless immutable
         because it is the primitive type or_null.
       But the kind of float or_null must be a subkind of any mod separable
         because it's the type argument to the array type.
|}, Principal{|
Line 1, characters 19-32:
1 | type should_fail = float or_null iarray
                       ^^^^^^^^^^^^^
Error: This type "float or_null" should be an instance of type
         "('a : any mod separable)"
       The kind of float or_null is value_or_null mod everything with float
         because it is the primitive type or_null.
       But the kind of float or_null must be a subkind of any mod separable
         because it's the type argument to the array type.
|}]

(* Test constructing iarray with or_null elements *)
let should_fail_iarray = [: Null; This 3.4 :]

[%%expect{|
Line 1, characters 39-42:
1 | let should_fail_iarray = [: Null; This 3.4 :]
                                           ^^^
Error: This expression has type "float" but an expression was expected of type
         "('a : value mod non_float)"
       The kind of float is value mod many unyielding stateless immutable
         because it is the primitive type float.
       But the kind of float must be a subkind of value mod non_float
         because it's the type of an array element.
|}]

(* List type arguments are [value_or_null]: *)
type t_any : any

type should_fail = t_any list

[%%expect{|
type t_any : any
Line 3, characters 19-24:
3 | type should_fail = t_any list
                       ^^^^^
Error: This type "t_any" should be an instance of type "('a : value_or_null)"
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be a sublayout of value
         because the type argument of list has layout value_or_null.
|}]

type t_value_or_null : value_or_null

type should_fail = t_value_or_null list

[%%expect{|
type t_value_or_null : value_or_null
type should_fail = t_value_or_null list
|}]

type t_any_mod_separable : any mod separable

type should_fail = t_any_mod_separable list

[%%expect{|
type t_any_mod_separable : any mod separable
Line 3, characters 19-38:
3 | type should_fail = t_any_mod_separable list
                       ^^^^^^^^^^^^^^^^^^^
Error: This type "t_any_mod_separable" should be an instance of type
         "('a : value_or_null)"
       The layout of t_any_mod_separable is any
         because of the definition of t_any_mod_separable at line 1, characters 0-44.
       But the layout of t_any_mod_separable must be a sublayout of value
         because the type argument of list has layout value_or_null.
|}]

type t_value : value

type should_work = t_value list

[%%expect{|
type t_value
type should_work = t_value list
|}]

let should_work_list = [ Null; This 3.4 ]

[%%expect{|
val should_work_list : float or_null list = [Null; This 3.4]
|}]

let should_work_list_of_list = [ [This 1; Null]; []; [This 2; This 3] ]

[%%expect{|
val should_work_list_of_list : int or_null list list =
  [[This 1; Null]; []; [This 2; This 3]]
|}]

(* Option type arguments are [value_or_null]: *)

type t_any : any

type should_fail = t_any option

[%%expect{|
type t_any : any
Line 3, characters 19-24:
3 | type should_fail = t_any option
                       ^^^^^
Error: This type "t_any" should be an instance of type "('a : value_or_null)"
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be a sublayout of value
         because the type argument of option has layout value_or_null.
|}]

type t_value_or_null : value_or_null

type should_fail = t_value_or_null option

[%%expect{|
type t_value_or_null : value_or_null
type should_fail = t_value_or_null option
|}]

type t_any_mod_separable : any mod separable

type should_fail = t_any_mod_separable option

[%%expect{|
type t_any_mod_separable : any mod separable
Line 3, characters 19-38:
3 | type should_fail = t_any_mod_separable option
                       ^^^^^^^^^^^^^^^^^^^
Error: This type "t_any_mod_separable" should be an instance of type
         "('a : value_or_null)"
       The layout of t_any_mod_separable is any
         because of the definition of t_any_mod_separable at line 1, characters 0-44.
       But the layout of t_any_mod_separable must be a sublayout of value
         because the type argument of option has layout value_or_null.
|}]

type t_value : value

type should_work = t_value option

[%%expect{|
type t_value
type should_work = t_value option
|}]

let should_work_option1 = Some (This 3.4)
let should_work_option2 = Some Null
let should_work_option3 = None

[%%expect{|
val should_work_option1 : float or_null option = Some (This 3.4)
val should_work_option2 :
  ('a : value_or_null mod non_null). 'a or_null option = Some Null
val should_work_option3 : 'a option = None
|}]

let should_work_list_option = [ Some (This 3.4); None; Some Null ]

[%%expect{|
val should_work_list_option : float or_null option list =
  [Some (This 3.4); None; Some Null]
|}]
