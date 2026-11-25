Use the production flag state and parsers with a synthetic two-feature registry.
Neither the compiler's registry nor the feature code is changed by these tests.

  $ export OCAMLPARAM=

Features start disabled; individual options apply in command-line order.

  $ ./selection_driver.exe
  sample-ai=false sample-human=false cfg-merge-blocks=false
  $ ./selection_driver.exe -experimental-sample-ai
  sample-ai=true sample-human=false cfg-merge-blocks=false
  $ ./selection_driver.exe -experimental-sample-human
  sample-ai=false sample-human=true cfg-merge-blocks=false
  $ ./selection_driver.exe -experimental-sample-ai -no-experimental-sample-ai
  sample-ai=false sample-human=false cfg-merge-blocks=false
  $ ./selection_driver.exe -no-experimental-sample-ai -experimental-sample-ai
  sample-ai=true sample-human=false cfg-merge-blocks=false

Universes select a minimum review level and replace previous selections.
Both the separate-argument and equals spellings are accepted.

  $ ./selection_driver.exe -experimental-universe ai-reviewed
  sample-ai=true sample-human=true cfg-merge-blocks=false
  $ ./selection_driver.exe -experimental-universe=human-reviewed
  sample-ai=false sample-human=true cfg-merge-blocks=false
  $ ./selection_driver.exe -experimental-sample-ai \
  >   -experimental-universe=human-reviewed
  sample-ai=false sample-human=true cfg-merge-blocks=false
  $ ./selection_driver.exe -experimental-universe=human-reviewed \
  >   -experimental-sample-ai
  sample-ai=true sample-human=true cfg-merge-blocks=false
  $ ./selection_driver.exe -experimental-universe=ai-reviewed \
  >   -no-experimental-sample-human
  sample-ai=true sample-human=false cfg-merge-blocks=false
  $ ./selection_driver.exe -no-experimental-sample-human \
  >   -experimental-universe=ai-reviewed
  sample-ai=true sample-human=true cfg-merge-blocks=false
  $ ./selection_driver.exe -experimental-universe=ai-reviewed \
  >   -experimental-universe=none
  sample-ai=false sample-human=false cfg-merge-blocks=false

Universe selection does not change ordinary flags. The testing preset does
not select a universe yet.

  $ ./selection_driver.exe -cfg-merge-blocks -experimental-universe=none
  sample-ai=false sample-human=false cfg-merge-blocks=true
  $ ./selection_driver.exe -test-optimizations
  sample-ai=false sample-human=false cfg-merge-blocks=true

OCAMLPARAM supports universes and positive/negative feature names, with 0 or 1.
Values before _ precede the arguments; values after _ override them.

  $ OCAMLPARAM='experimental-universe=ai-reviewed,_' \
  >   ./selection_driver.exe -experimental-universe=none
  sample-ai=false sample-human=false cfg-merge-blocks=false
  $ OCAMLPARAM='_,experimental-universe=ai-reviewed' \
  >   ./selection_driver.exe -experimental-universe=none
  sample-ai=true sample-human=true cfg-merge-blocks=false
  $ OCAMLPARAM='experimental-sample-ai=1,_' \
  >   ./selection_driver.exe -no-experimental-sample-ai
  sample-ai=false sample-human=false cfg-merge-blocks=false
  $ OCAMLPARAM='_,experimental-sample-ai=0' \
  >   ./selection_driver.exe -experimental-sample-ai
  sample-ai=false sample-human=false cfg-merge-blocks=false
  $ OCAMLPARAM='_,no-experimental-sample-ai=1' \
  >   ./selection_driver.exe -experimental-sample-ai
  sample-ai=false sample-human=false cfg-merge-blocks=false
  $ OCAMLPARAM='_,no-experimental-sample-ai=0' ./selection_driver.exe
  sample-ai=true sample-human=false cfg-merge-blocks=false
  $ OCAMLPARAM='experimental-universe=ai-reviewed,_,experimental-sample-ai=0' \
  >   ./selection_driver.exe
  sample-ai=false sample-human=true cfg-merge-blocks=false
  $ OCAMLPARAM='_,experimental-sample-ai=1,experimental-universe=none' \
  >   ./selection_driver.exe
  sample-ai=false sample-human=false cfg-merge-blocks=false
  $ OCAMLPARAM='_,experimental-universe=none,experimental-sample-ai=1' \
  >   ./selection_driver.exe
  sample-ai=true sample-human=false cfg-merge-blocks=false

Invalid boolean values warn without changing the selection. Unknown parameter
names use the compiler's existing discarded-parameter diagnostic.

  $ OCAMLPARAM='_,experimental-sample-ai=invalid' \
  >   ./selection_driver.exe -experimental-sample-ai
  File "_none_", line 1:
  Warning 46 [bad-env-variable]: illegal environment variable OCAMLPARAM : bad value invalid for experimental-sample-ai
  sample-ai=true sample-human=false cfg-merge-blocks=false
  $ OCAMLPARAM='_,no-experimental-sample-ai=invalid' \
  >   ./selection_driver.exe -experimental-sample-ai
  File "_none_", line 1:
  Warning 46 [bad-env-variable]: illegal environment variable OCAMLPARAM : bad value invalid for no-experimental-sample-ai
  sample-ai=true sample-human=false cfg-merge-blocks=false
  $ OCAMLPARAM='_,experimental-missing=1' ./selection_driver.exe 2> warning
  sample-ai=false sample-human=false cfg-merge-blocks=false
  $ sed '/^$/d' warning
  File "_none_", line 1:
  Warning 46 [bad-env-variable]: illegal environment variable OCAMLPARAM : Warning: discarding value of variable "experimental-missing" in OCAMLPARAM

Invalid universe values and unknown command-line features are rejected.

  $ ./selection_driver.exe -experimental-universe=invalid 2> error
  [2]
  $ head -1 error
  ./selection_driver.exe: wrong argument 'invalid'; option '-experimental-universe=invalid' expects one of: none ai-reviewed human-reviewed.
  $ OCAMLPARAM='experimental-universe=invalid,_' ./selection_driver.exe
  Unknown experimental universe "invalid" (expected one of: none, ai-reviewed, human-reviewed)
  [2]
  $ ./selection_driver.exe -experimental-missing 2> error
  [2]
  $ head -1 error
  ./selection_driver.exe: unknown option '-experimental-missing'.
  $ ./selection_driver.exe -no-experimental-missing 2> error
  [2]
  $ head -1 error
  ./selection_driver.exe: unknown option '-no-experimental-missing'.

Help includes both generated feature switches and the universe selector.

  $ ./selection_driver.exe -help | grep -E -- ' -([a-z-]*experimental-)'
    -experimental-universe {none|ai-reviewed|human-reviewed} Select registered experimental features by minimum review level (default: none)
    -experimental-sample-ai  Enable experimental feature sample-ai
    -no-experimental-sample-ai  Disable experimental feature sample-ai
    -experimental-sample-human  Enable experimental feature sample-human
    -no-experimental-sample-human  Disable experimental feature sample-human
