#!/bin/sh

can_link_gfortran () {
  echo 'int main(){}' > test_compile.c
  gcc -o test_compile test_compile.c -lgfortran >/dev/null 2>&1
  ret=$?
  rm -f test_compile test_compile.c
  return $ret
}

if ! which gfortran > /dev/null 2>&1; then
  echo "gfortran not available" > ${ocamltest_response}
  test_result=${TEST_SKIP}
elif ! echo "${cc}" | grep -q '^gcc'; then
  echo "OCaml was not compiled with gcc" > ${ocamltest_response}
  test_result=${TEST_SKIP}
elif gcc --version 2>&1 | grep -q 'Apple clang version'; then
  echo "OCaml was not compiled with gcc" > ${ocamltest_response}
  test_result=${TEST_SKIP}
elif ! can_link_gfortran; then
  echo "-lgfortran unavailable" > ${ocamltest_response}
  test_result=${TEST_SKIP}
else
  test_result=${TEST_PASS}
fi

exit ${test_result}
