(* TEST
 flags = "-dsource -extension mode_polymorphism_alpha";
 expect;
*)

module type Variables = sig
  val f : 'a @ 'm -> 'a @ 'm
end
[%%expect{|

module type Variables  = sig val f : 'a @ 'm -> 'a @ 'm end;;
>> Fatal error: Line 2, characters 15-17: mode variables and mode bounds are not yet supported
Uncaught exception: Misc.Fatal_error

|}]

module type Upper_and_lower = sig
  val f : 'a @ [< 'm & 'n] -> 'a @ [> 'm | 'n]
end
[%%expect{|

module type Upper_and_lower  =
  sig val f : 'a @ [< 'm & 'n] -> 'a @ [> 'm | 'n] end;;
>> Fatal error: Line 2, characters 15-26: mode variables and mode bounds are not yet supported
Uncaught exception: Misc.Fatal_error

|}]

module type Constants = sig
  val f : 'a @ [< portable many] -> 'a @ [> local | once]
end
[%%expect{|

module type Constants  =
  sig val f : 'a @ [< portable many] -> 'a @ [> local | once] end;;
>> Fatal error: Line 2, characters 15-32: mode variables and mode bounds are not yet supported
Uncaught exception: Misc.Fatal_error

|}]

module type Constants_after_variables = sig
  val f : 'a @ [< 'm & portable many] -> 'a @ [> 'm | aliased dynamic]
end
[%%expect{|

module type Constants_after_variables  =
  sig val f : 'a @ [< 'm & portable many] -> 'a @ [> 'm | aliased dynamic]
  end;;
>> Fatal error: Line 2, characters 15-37: mode variables and mode bounds are not yet supported
Uncaught exception: Misc.Fatal_error

|}]

module type Constants_after_mod = sig
  val f :
    'a @ [< 'm mod portable contended & global many] ->
    'a @ [> 'm mod aliased dynamic | local once]
end
[%%expect{|

module type Constants_after_mod  =
  sig
    val f :
      'a @ [< 'm mod portable contended & global many] ->
        'a @ [> 'm mod aliased dynamic | local once]
  end;;
>> Fatal error: Line 3, characters 9-52: mode variables and mode bounds are not yet supported
Uncaught exception: Misc.Fatal_error

|}]

module type Any_order = sig
  val f : 'a @ [< portable & 'm & many] -> 'a @ [> local | 'm | once]
end
[%%expect{|

module type Any_order  =
  sig val f : 'a @ [< portable & 'm & many] -> 'a @ [> local | 'm | once] end;;
>> Fatal error: Line 2, characters 15-39: mode variables and mode bounds are not yet supported
Uncaught exception: Misc.Fatal_error

|}]

module type Combined = sig
  val f : 'a @ [< 'm & portable > 'n | dynamic] -> 'a @ [> 'm]
end
[%%expect{|

module type Combined  =
  sig val f : 'a @ [< 'm & portable > 'n | dynamic] -> 'a @ [> 'm] end;;
>> Fatal error: Line 2, characters 15-47: mode variables and mode bounds are not yet supported
Uncaught exception: Misc.Fatal_error

|}]

module type Combined_lower_first = sig
  val f : 'a @ [> 'n | dynamic < 'm & portable] -> 'a @ [> 'm]
end
[%%expect{|

module type Combined_lower_first  =
  sig val f : 'a @ [< 'm & portable > 'n | dynamic] -> 'a @ [> 'm] end;;
>> Fatal error: Line 2, characters 15-47: mode variables and mode bounds are not yet supported
Uncaught exception: Misc.Fatal_error

|}]

module type Mixed_consts = sig
  val f : 'a @ [> portable | 'm | nonportable] -> 'a @ [< 'm]
end
[%%expect{|

module type Mixed_consts  =
  sig val f : 'a @ [> portable | 'm | nonportable] -> 'a @ [< 'm] end;;
>> Fatal error: Line 2, characters 15-46: mode variables and mode bounds are not yet supported
Uncaught exception: Misc.Fatal_error

|}]

module type Morphisms_and_mod = sig
  val f :
    'a @ [< 'm mod portable contended & past('n) mod global] ->
    'a @ [> close('m) mod many aliased | past('n) | local once]
end
[%%expect{|

module type Morphisms_and_mod  =
  sig
    val f :
      'a @ [< 'm mod portable contended & past('n) mod global] ->
        'a @ [> close('m) mod many aliased | past('n) | local once]
  end;;
>> Fatal error: Line 3, characters 9-60: mode variables and mode bounds are not yet supported
Uncaught exception: Misc.Fatal_error

|}]
