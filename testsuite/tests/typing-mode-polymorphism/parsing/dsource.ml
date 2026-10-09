(* TEST
 flags = "-dsource -extension mode_polymorphism_alpha";
 expect;
*)

module type Variables = sig
  val f : 'a @ 'm -> 'a @ 'm
end
[%%expect{|

module type Variables  = sig val f : 'a @ 'm -> 'a @ 'm end;;
module type Variables = sig val f : 'a -> 'a end
|}]

module type Upper_and_lower = sig
  val f : 'a @ [< 'm & 'n] -> 'a @ [> 'm | 'n]
end
[%%expect{|

module type Upper_and_lower  =
  sig val f : 'a @ [< 'm & 'n] -> 'a @ [> 'm | 'n] end;;
module type Upper_and_lower = sig val f : 'a -> 'a end
|}]

module type Constants = sig
  val f : 'a @ [< portable many] -> 'a @ [> local | once]
end
[%%expect{|

module type Constants  =
  sig val f : 'a @ [< portable & many] -> 'a @ [> local | once] end;;
module type Constants = sig val f : 'a @ portable -> 'a @ local once end
|}]

module type Constants_after_variables = sig
  val f : 'a @ [< 'm & portable many] -> 'a @ [> 'm | aliased dynamic]
end
[%%expect{|

module type Constants_after_variables  =
  sig
    val f : 'a @ [< 'm & portable & many] -> 'a @ [> 'm | aliased | dynamic]
  end;;
module type Constants_after_variables = sig val f : 'a @ portable -> 'a end
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
      'a @ [< 'm mod portable contended & global & many] ->
        'a @ [> 'm mod aliased dynamic | local | once]
  end;;
module type Constants_after_mod = sig val f : 'a -> 'a @ local once end
|}]

module type Any_order = sig
  val f : 'a @ [< portable & 'm & many] -> 'a @ [> local | 'm | once]
end
[%%expect{|

module type Any_order  =
  sig val f : 'a @ [< portable & 'm & many] -> 'a @ [> local | 'm | once] end;;
module type Any_order = sig val f : 'a @ portable -> 'a @ local once end
|}]

module type Combined = sig
  val f : 'a @ [< 'm & portable > 'n | dynamic] -> 'a @ [> 'm]
end
[%%expect{|

module type Combined  =
  sig val f : 'a @ [< 'm & portable > 'n | dynamic] -> 'a @ [> 'm] end;;
module type Combined = sig val f : 'a @ portable -> 'a end
|}]

module type Combined_lower_first = sig
  val f : 'a @ [> 'n | dynamic < 'm & portable] -> 'a @ [> 'm]
end
[%%expect{|

module type Combined_lower_first  =
  sig val f : 'a @ [< 'm & portable > 'n | dynamic] -> 'a @ [> 'm] end;;
module type Combined_lower_first = sig val f : 'a @ portable -> 'a end
|}]

module type Mixed_consts = sig
  val f : 'a @ [> portable | 'm | nonportable] -> 'a @ [< 'm]
end
[%%expect{|

module type Mixed_consts  =
  sig val f : 'a @ [> portable | 'm | nonportable] -> 'a @ [< 'm] end;;
Line 2, characters 34-45:
2 |   val f : 'a @ [> portable | 'm | nonportable] -> 'a @ [< 'm]
                                      ^^^^^^^^^^^
Error: The portability axis has already been specified.
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
        'a @ [> close('m) mod many aliased | past('n) | local | once]
  end;;
module type Morphisms_and_mod = sig val f : 'a -> 'a @ local once end
|}]
