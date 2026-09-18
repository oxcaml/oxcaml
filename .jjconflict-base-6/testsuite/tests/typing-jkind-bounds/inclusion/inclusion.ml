type ('a, 'b) u : value mod portable with 'b

type ('a
     , 'b
     , 'c)
     t :
     value mod portable with 'a with 'b with ('b, 'c) u with ('a * 'b, 'c) u
