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
    ~name:"cycle"
    ~needle:"cycle"
    ~source:
      {|
L = [
  A < B;
  B < A
]
|};
  Test_support.expect_error
    ~name:"missing-extreme"
    ~needle:"missing a unique bottom"
    ~source:
      {|
L = [
  A;
  B
]
|};
  Test_support.expect_error
    ~name:"not-lattice"
    ~needle:"join"
    ~source:
      {|
L = [
  Bot < A < U < Top;
  Bot < A < V < Top;
  Bot < B < U;
  Bot < B < V
]
|};
  Test_support.expect_error
    ~name:"nondistributive"
    ~needle:"not distributive"
    ~source:
      {|
L = [
  Bot < A < Top;
  Bot < B < Top;
  Bot < C < Top
]
|};
  Test_support.expect_error
    ~name:"bad-field"
    ~needle:"unknown lattice"
    ~source:
      {|
A = [ X < Y ]
B = { bad : Missing }
|};
  Test_support.expect_error
    ~name:"bad-embedding"
    ~needle:"injective"
    ~source:
      {|
S = [ A < B ]
T = [ X < Y < Z ]
S <= T via {
  A -> X;
  B -> X;
}
|};
  Test_support.expect_error
    ~name:"bad-alias"
    ~needle:"missing function"
    ~source:
      {|
S = [ A < B ]
T = [ X < Y < Z ]
S <= T via {
  A -> X;
  B -> Z;
} aliases {
  left9 = Nope;
}
|}
