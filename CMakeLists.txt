cmake_minimum_required(VERSION 3.14)
project(
  unittest
  DESCRIPTION "Fortran90 unittest"
  LANGUAGES Fortran)

include(GNUInstallDirs)

cmake_policy(SET CMP0076 NEW)

enable_language(Fortran)
if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU AND CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 9.0)
  message(FATAL_ERROR "GCC 9 or newer required.")
endif()

if(DEFINED CMAKE_MAXIMUM_RANK AND NOT DEFINED UNITTEST_MAXIMUM_RANK)
  message(DEPRECATION "CMAKE_MAXIMUM_RANK is deprecated; use UNITTEST_MAXIMUM_RANK instead.")
  set(UNITTEST_MAXIMUM_RANK "${CMAKE_MAXIMUM_RANK}" CACHE STRING "Maximum array rank supported by unittest")
endif()
if(NOT DEFINED UNITTEST_MAXIMUM_RANK)
  set(UNITTEST_MAXIMUM_RANK 1 CACHE STRING "Maximum array rank supported by unittest")
endif()
if(NOT UNITTEST_MAXIMUM_RANK MATCHES "^[0-9]+$" OR UNITTEST_MAXIMUM_RANK GREATER 15)
  message(FATAL_ERROR "UNITTEST_MAXIMUM_RANK must be an integer between 0 and 15.")
endif()

find_program(FYPP fypp)
if(NOT FYPP)
  message(FATAL_ERROR "FYPP not found.")
endif()

set(fyppFlags)

include(CheckFortranSourceCompiles)
include(CheckFortranSourceRuns)
check_fortran_source_runs(
  "if (selected_real_kind(3) == selected_real_kind(6)) STOP 1; end"
  HP_SUPPORT
  )
check_fortran_source_runs(
  "if (selected_real_kind(33) == -1) STOP 1; end"
  QP_SUPPORT
  )
check_fortran_source_runs(
  "if (ANY(selected_real_kind(18) == [-1, selected_real_kind(33)])) STOP 1; end"
  XDP_SUPPORT
  )

list(
  APPEND fyppFlags
  "-DHP_SUPPORT=$<BOOL:${HP_SUPPORT}>"
  "-DXDP_SUPPORT=$<BOOL:${XDP_SUPPORT}>"
  "-DQP_SUPPORT=$<BOOL:${QP_SUPPORT}>"
  "-DMAXIMUM_RANK=${UNITTEST_MAXIMUM_RANK}"
  "-DWIN32=$<BOOL:${WIN32}>"
  "-DUNIX=$<BOOL:${UNIX}>"
  "-I${PROJECT_SOURCE_DIR}/include"
  )

if(CMAKE_SOURCE_DIR STREQUAL PROJECT_SOURCE_DIR)
  include(CTest)
  set(_unittest_build_testing_default ${BUILD_TESTING})
else()
  set(_unittest_build_testing_default OFF)
endif()
option(UNITTEST_BUILD_TESTING "Build and register unittest's own tests" ${_unittest_build_testing_default})

add_subdirectory(src)
if(UNITTEST_BUILD_TESTING)
  if(NOT BUILD_TESTING)
    enable_testing()
  endif()
  add_subdirectory(test)
endif()
