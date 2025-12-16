# ikinds todos

Optimize algorithm: 
- only substitute LDD variables that are now concrete in the sense that they now have a manifest (abstracts have been fully normalized)
- when normalizing a lhs kind, set all abstract types that don't occur on the right to top
  - in mode crossing, essentially the rhs is just plain mod bounds, which don't have any abstract types, so ALL abstract LDD variables can be considered top for mode crossing (for non-variables we do need to meet in the bounds of abstract types)
- remove ctx closure indirection
- go over all algorithms to spot micro optimizations
- preserve ikinds in more places
- limit types hash table to types that are potentially circular (i.e., polymorphic variants) -- but keep constructor path table because that can be generally mutually recursive
- subtract-normalize parameterized type ikinds (subtract base from coeffs)
- use bitfield in mode solver
- add a mode for no jkind and mode histories / provenance

Measurement:
- measure performance on tree
- measure performance on compiler

Correctness:
- investigate lookup failure (add asserts)
- add more tests

Next steps:
- rebase on abstract kinds
- error printing from ikinds
- remove jkinds entirely
