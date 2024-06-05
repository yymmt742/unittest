cmake_minimum_required(VERSION 3.14)
project(
  unittest
  DESCRIPTION "Fortran90 unittest"
  LANGUAGES Fortran)

cmake_policy(SET CMP0076 NEW)

enable_language(Fortran)
if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU AND CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 9.0)
  message(FATAL_ERROR "GCC 9 or newer required.")
endif()

if(NOT DEFINED CMAKE_MAXIMUM_RANK)
  set(CMAKE_MAXIMUM_RANK 1 CACHE STRING "Maxmum array rank")
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
  "-DMAXIMUM_RANK=${CMAKE_MAXIMUM_RANK}"
  "-DWIN32=$<BOOL:${WIN32}>"
  "-DUNIX=$<BOOL:${UNIX}>"
  "-I${PROJECT_SOURCE_DIR}/include"
  )

enable_testing()
include(CTest)

add_subdirectory(src)
add_subdirectory(test)
