# Read after ../stdlib/StdlibModules, which defines STDLIB_MODULE_BASENAMES.
stdlib-modules: ; @echo stdlib_modules; printf ' %s\n' $(STDLIB_MODULE_BASENAMES)
