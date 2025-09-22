//Provides: manyargs
function manyargs(a, b, c, d, e, f, g, h, i, j, k) {
  var console = globalThis.console;
  console.log("a = " + a);
  console.log("b = " + b);
  console.log("c = " + c);
  console.log("d = " + d);
  console.log("e = " + e);
  console.log("f = " + f);
  console.log("g = " + g);
  console.log("h = " + h);
  console.log("i = " + i);
  console.log("j = " + j);
  console.log("k = " + k);
  return 0;
}

//Provides: manyargs_argv
//Requires: manyargs
function manyargs_argv(a, b, c, d, e, f, g, h, i, j, k) {
  return manyargs(a, b, c, d, e, f, g, h, i, j, k);
}
