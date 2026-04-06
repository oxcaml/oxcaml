open Test_support

let run () =
  let x = Bitwise.var ~mask:15 "x" in
  let y = Bitwise.var ~mask:15 "y" in
  ensure
    (Bitwise.render (Bitwise.shift_l x 0) = "x")
    "shift_l 0 should simplify";
  ensure
    (Bitwise.render (Bitwise.shift_r x 0) = "x")
    "shift_r 0 should simplify";
  ensure
    (Bitwise.render (Bitwise.mask (Bitwise.var ~mask:3 "x") 3) = "x")
    "redundant mask should simplify";
  ensure
    (Bitwise.render (Bitwise.xor_mask (Bitwise.var ~mask:3 "x") 3) = "x lxor 0b11")
    "xor_mask should render as lxor";
  ensure
    (Bitwise.render
       (Bitwise.and_
          [ Bitwise.var ~mask:3 "x";
            Bitwise.xor_mask (Bitwise.var ~mask:3 "y") 3
          ])
     = "x land lnot y")
    "xor_mask under a full conjunction should render as lnot";
  ensure
    (Bitwise.render (Bitwise.xor_ (Bitwise.var ~mask:3 "x") (Bitwise.var ~mask:3 "y"))
     = "x lxor y")
    "binary xor should render as lxor";
  ensure
    (Bitwise.render (Bitwise.xor_mask (Bitwise.xor_mask (Bitwise.var ~mask:3 "x") 3) 1)
     = "x lxor 0b10")
    "nested xor_mask should compose";
  ensure
    (Bitwise.render (Bitwise.or_ [ Bitwise.const 1; Bitwise.const 2; Bitwise.var ~mask:1 "x" ])
     = "0b11 lor x")
    "or_ should flatten and merge constants";
  ensure
    (Bitwise.render (Bitwise.shift_r (Bitwise.var ~mask:15 "x") 1) = "x lsr 1")
    "shift_r should drop redundant low-bit masks";
  let mixed_xor_or =
    Bitwise.render
      (Bitwise.or_
         [ Bitwise.xor_mask (Bitwise.var ~mask:3 "x") 3;
           Bitwise.var ~mask:3 "y"
         ])
  in
  ensure
    (mixed_xor_or = "y lor (x lxor 0b11)" || mixed_xor_or = "(x lxor 0b11) lor y")
    "mixed xor/or should be parenthesized safely: %s"
    mixed_xor_or;
  let sub = Bitwise.and_ [ x; y ] in
  let bindings, body =
    Bitwise.cse
      (Bitwise.or_ [ Bitwise.shift_l sub 1; Bitwise.shift_r sub 1 ])
  in
  ensure (List.length bindings = 1) "expected one extracted binding";
  let binding = List.hd bindings in
  ensure
    (Bitwise.render binding.expr = "x land y")
    "unexpected extracted binding: %s"
    (Bitwise.render binding.expr);
  ensure
    (String.length binding.name > 0)
    "binding name should not be empty";
  let expected_body =
    let tmp = Bitwise.var ~mask:(Bitwise.mask_of binding.expr) binding.name in
    Bitwise.render
      (Bitwise.or_ [ Bitwise.shift_l tmp 1; Bitwise.shift_r tmp 1 ])
  in
  ensure
    (Bitwise.render body = expected_body)
    "unexpected rewritten body: %s"
    (Bitwise.render body);
  let x = Bitwise.var ~mask:32767 "x" in
  let y = Bitwise.var ~mask:32767 "y" in
  let mixed_join =
    Bitwise.simplify
      (Bitwise.or_
         [ Bitwise.and_ [ x; Bitwise.const 511 ];
           Bitwise.and_ [ y; Bitwise.const 511 ];
           Bitwise.and_
             [ Bitwise.and_ [ x; Bitwise.const 32256 ];
               Bitwise.and_ [ y; Bitwise.const 32256 ]
             ]
         ])
      32767
  in
  ensure
    (Bitwise.render mixed_join
     = "((x land y) land 0b111111000000000) lor ((x lor y) land 0b111111111)")
    "unexpected mixed join simplification: %s"
    (Bitwise.render mixed_join);
  let mixed_meet =
    Bitwise.simplify
      (Bitwise.or_
         [ Bitwise.and_
             [ Bitwise.and_ [ x; Bitwise.const 511 ];
               Bitwise.and_ [ y; Bitwise.const 511 ]
             ];
           Bitwise.and_ [ x; Bitwise.const 32256 ];
           Bitwise.and_ [ y; Bitwise.const 32256 ]
         ])
      32767
  in
  ensure
    (Bitwise.render mixed_meet
     = "((x lor y) land 0b111111000000000) lor ((x land y) land 0b111111111)")
    "unexpected mixed meet simplification: %s"
    (Bitwise.render mixed_meet)
