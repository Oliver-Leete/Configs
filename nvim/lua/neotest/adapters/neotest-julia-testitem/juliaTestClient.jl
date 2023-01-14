using TestItemRunner

@run_package_tests filter=ti->(ti.name == ARGS[1]) verbose=true
