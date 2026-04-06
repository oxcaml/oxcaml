let run () =
  Test_support.expect_error
    ~name:"mixed"
    ~needle:"same direction"
    ~source:
      {|
L = [
  A < B;
  C > D
]
|};
  Test_support.expect_error
    ~name:"non-monotone"
    ~needle:"not monotone"
    ~source:
      {|
A = [ Lo < Hi ]
B = [ Red < Blue ]
bad : A -> B = [
  Lo -> Blue;
  Hi -> Red;
]
|};
  Test_support.expect_error
    ~name:"unknown-bridge-field"
    ~needle:"unknown source field"
    ~source:
      {|
A = [ Lo < Hi ]
P = { x : A }
Q = { y : A }
bad : P -> Q = {
  y = z;
}
|};
  Test_support.expect_error
    ~name:"duplicate-target-field"
    ~needle:"duplicate assignment"
    ~source:
      {|
A = [ Lo < Hi ]
P = { x : A }
Q = { y : A }
bad : P -> Q = {
  y = x;
  y = min;
}
|};
  Test_support.expect_error
    ~name:"missing-target-field"
    ~needle:"missing assignment"
    ~source:
      {|
A = [ Lo < Hi ]
P = { x : A }
Q = { y : A; z : A }
bad : P -> Q = {
  y = x;
}
|};
  Test_support.expect_error
    ~name:"product-op-mismatch"
    ~needle:"expects A but source field"
    ~source:
      {|
A = [ Lo < Hi ]
P = { x : A }
Q = { y : A }
bad : P^op -> Q = {
  y = x;
}
|};
  Test_support.expect_error
    ~name:"join-type-mismatch"
    ~needle:"field \"z\" expects A but source field \"y\" has B"
    ~source:
      {|
A = [ Lo < Hi ]
B = [ Red < Blue ]
P = { x : A; y : B }
Q = { z : A }
bad : P -> Q = {
  z = join(x, y);
}
|}
