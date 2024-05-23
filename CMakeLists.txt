cmake_minimum_required(VERSION 3.14)
project(
  unittest
  DESCRIPTION "Fortran90 unittest"
  LANGUAGES Fortran)

cmake_policy(SET CMP0076 NEW)

enable_language(Fortran)
include(CheckFortranSourceCompiles)
include(CheckFortranSourceRuns)

add_subdirectory(src)
if(CMAKE_BUILD_TYPE MATCHES "Debug")
  enable_testing()
  include(CTest)
  add_subdirectory(test)
endif()
