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
|};
  Test_support.expect_error
    ~name:"adjoint_chain_unknown"
    ~needle:"unknown morph"
    ~source:
      {|
A = [ Lo < Hi ]
B = [ Red < Blue ]
f : A -> B = [
  Lo -> Red;
  Hi -> Blue;
]
f -| g
|};
  Test_support.expect_error
    ~name:"adjoint_chain_syntax"
    ~needle:"syntax error"
    ~source:
      {|
A = [ Lo < Hi ]
B = [ Red < Blue ]
f : A -> B = [
  Lo -> Red;
  Hi -> Blue;
]
f -|
|};
  Test_support.expect_error
    ~name:"adjoint_chain_type_mismatch"
    ~needle:"type mismatch"
    ~source:
      {|
A = [ Lo < Hi ]
B = [ Red < Blue ]
f : A -> B = [
  Lo -> Red;
  Hi -> Blue;
]
id_a : A -> A = [
  Lo -> Lo;
  Hi -> Hi;
]
f -| id_a
|};
  Test_support.expect_error
    ~name:"adjoint_chain_missing_right"
    ~needle:"has no right adjoint"
    ~source:
      {|
A = [ Lo < Hi ]
Pair = {
  left : A;
  right : A;
}
Single = {
  value : A;
}
meet_pair : Pair -> Single = {
  value = meet(left, right);
}
dup : Single -> Pair = {
  left = value;
  right = value;
}
meet_pair -| dup
|};
  Test_support.expect_error
    ~name:"adjoint_chain_wrong_adjoint"
    ~needle:"is not left adjoint to"
    ~source:
      {|
A = [ Lo < Hi ]
Pair = {
  left : A;
  right : A;
}
Single = {
  value : A;
}
join_pair : Pair -> Single = {
  value = join(left, right);
}
left_only : Single -> Pair = {
  left = value;
  right = min;
}
join_pair -| left_only
|}
