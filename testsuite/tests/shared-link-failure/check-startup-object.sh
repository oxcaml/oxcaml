if [ -f startup_object.cmxs.startup.o ]
then
    exit ${TEST_FAIL}
else
    exit ${TEST_PASS}
fi
