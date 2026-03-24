module R = Runner(Elem)(Int_elem) [@jane.non_erasable.instances]
let () = R.run [1; 2; 3; 4; 5]
