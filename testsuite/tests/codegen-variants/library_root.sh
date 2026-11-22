# For the scripts of the tests that need a library root of their own; to be
# sourced.
#
# make_library_root <library root> <variant>
#
# Makes ./root, a library root with the contents of <library root> for the
# default variant and for <variant>, and sets $selected to the tree in it
# that a compilation for <variant> uses: root/variants/<variant>, or root
# itself when <variant> is the default one.  The directories of both trees
# are new ones, at every depth, and the files in them are symbolic links
# to the files of <library root>.  So a test can add files anywhere in
# both trees, and what it adds stays out of <library root>.  To replace a
# file it removes the link first: writing to the link would change the
# file of <library root>.

# link_tree <directory> <new directory>
#
# Makes <new directory> with the directories of <directory>, but for
# variants, and puts in each of them a symbolic link to every file of the
# original that is not a directory.
link_tree () {
  (cd "$1" && find -L . -path ./variants -prune -o -type d -print) |
  while IFS= read -r directory; do
    mkdir -p "$2/$directory" || exit 1
    (cd "$2/$directory" &&
     find -L "$1/$directory/." ! -name . -prune ! -type d \
       -exec sh -c 'ln -s "$@" .' sh {} +) || exit 1
  done
}

make_library_root () {
  library_root=$(cd "$1" && pwd)
  link_tree "$library_root" "$(pwd)/root"
  mkdir root/variants
  if [ -d "$library_root/variants/$2" ]; then
    selected=root/variants/$2
    link_tree "$library_root/variants/$2" "$(pwd)/$selected"
  else
    selected=root
  fi
}
