/* Test file 2: function and data */

int global_value = 42;
static int static_value = 100;

int multiply(int a, int b) {
    return a * b + global_value + static_value;
}
