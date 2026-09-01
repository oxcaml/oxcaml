gencopy is a dev utility tool that we use to generate migration functions
skeletons when adding support for new compiler versions.

It generates deep copy functions from the types defined in the provided
ast_X.ml to what it assumes is in ast_Y.ml. The latter is only used to for
type annotations and qualified record fiels/variant names. Those functions
are generated assuming types are symmetrical across the two files.

One would first generate `migrate_X_Y.ml` and `migrate_Y_X.ml` by running
respectively:
```
gencopy ast_X.ml ast_Y.ml > migrate_X_Y.ml
```
and
```
gencopy ast_Y.ml ast_X.ml > migrate_Y_X.ml
```
from the `astlib/` folder.

Trying to build ppxlib should give you compile errors for those two files
corresponding to the specific parts of the AST types that changed across the two
versions.

Properly fixing those until it all builds should give you a valid set of
migration functions.
