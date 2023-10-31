add_compile_options(
  $<$<COMPILE_LANG_AND_ID:C,GNU>:-O2>
  $<$<COMPILE_LANG_AND_ID:C,GNU>:-Wall>
  $<$<COMPILE_LANG_AND_ID:C,GNU>:-Wextra>
  )

add_compile_options(
  $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-O2>
  $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-std=f2008>
  $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-fimplicit-none>
  $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-fall-intrinsics>
  $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-fexternal-blas>
  $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-O2>
  $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-stand>
  $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:f08>
  $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-free>
  $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-implicitnone>
  $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-qopt-matmul>
  )

if( CMAKE_BUILD_TYPE MATCHES "Debug" )

  add_compile_options(
    $<$<COMPILE_LANG_AND_ID:C,GNU>:-g>
    $<$<COMPILE_LANG_AND_ID:C,GNU>:-Wshadow>
    $<$<COMPILE_LANG_AND_ID:C,GNU>:-Wconversion>
    $<$<COMPILE_LANG_AND_ID:C,GNU>:-Wfloat-equal>
    $<$<COMPILE_LANG_AND_ID:C,GNU>:-ftrapv>
    $<$<COMPILE_LANG_AND_ID:C,GNU>:-fstack-protector-all>
    )
  add_compile_options(
    $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-Wuninitialized>
    $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-fbounds-check>
    $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-Wunused>
    $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-fbacktrace>
    $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-fcheck=array-temps,bounds,do,mem,pointer,recursion>
    $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-warn>
    $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:all>
    $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-check>
    $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:bounds>
    $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-traceback>
  )

endif()

add_library( unittest STATIC unittest.f90 )

set(LIB_MOD_DIR ${CMAKE_CURRENT_BINARY_DIR}/mod_files/)

if(NOT EXISTS "${LIB_MOD_DIR}")
  make_directory("${LIB_MOD_DIR}")
endif()

set_target_properties(${PROJECT_NAME} PROPERTIES
  Fortran_MODULE_DIRECTORY ${LIB_MOD_DIR})
target_include_directories(${PROJECT_NAME} PUBLIC
  $<BUILD_INTERFACE:${LIB_MOD_DIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_MODULEDIR}>
  )

if( CMAKE_BUILD_TYPE MATCHES "Debug" )

  add_executable( test_unittest test_unittest.f90 )
  target_link_libraries( test_unittest PRIVATE unittest )
  add_test( NAME test_unittest COMMAND test_unittest )

endif()

install(TARGETS unittest
  EXPORT unittest-export
  ARCHIVE DESTINATION "${CMAKE_INSTALL_PREFIX}/lib"
  LIBRARY DESTINATION "${CMAKE_INSTALL_PREFIX}/lib"
  )

install(DIRECTORY ${LIB_MOD_DIR}
  DESTINATION "${CMAKE_INSTALL_PREFIX}/cmake/${PROJECT_NAME}/mod_files/"
  )

install(EXPORT ${PROJECT_NAME}-export
  FILE unittest-config.cmake
  DESTINATION "${CMAKE_INSTALL_PREFIX}/cmake/${PROJECT_NAME}")

#get_cmake_property(_variableNames CACHE_VARIABLES)
#list (SORT _variableNames)
#foreach (_variableName ${_variableNames})
#      message(STATUS "${_variableName}=${${_variableName}}")
#endforeach()
