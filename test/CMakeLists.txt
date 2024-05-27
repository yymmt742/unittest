add_executable(test_unittest test_unittest.f90)
target_include_directories(test_unittest PRIVATE ${LIB_MOD_DIR})
target_link_libraries(test_unittest PRIVATE unittest)
add_test(NAME test_unittest COMMAND test_unittest)
