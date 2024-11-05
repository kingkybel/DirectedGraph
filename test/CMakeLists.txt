add_executable(run_tests
        run_tests.cc
        directed_graph_tests.cc
        directed_graph_traits_tests.cc
)

target_link_libraries(run_tests
        gtest
        gtest_main
)
add_test(NAME run_tests COMMAND run_tests)

