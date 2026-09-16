if [ -f test_stop_after.cmo ]
then
    exit ${TEST_PASS}
else
    exit ${TEST_FAIL}
fi
