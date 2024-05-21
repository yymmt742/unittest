find_program(FYPP fypp)
if(NOT FYPP)
  message(FATAL_ERROR "FYPP not found.")
endif()

list(APPEND fyppFiles
  assert.fypp
  equal.fypp
  compare.fypp
  )
set(fyppFlags)

foreach(fyppfile ${fyppFiles})
  string(REGEX REPLACE "\\.fypp" ".f90" f90file ${fyppfile})
  string(REGEX REPLACE "\\.fypp" ".h" incfile ${fyppfile})
  string(REGEX REPLACE "\\.fypp" ".h.fypp" incsrc ${fyppfile})
  add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${incfile}
    COMMAND fypp "${fyppFlags}" ${CMAKE_CURRENT_SOURCE_DIR}/${incsrc}
            ${CMAKE_CURRENT_BINARY_DIR}/${incfile}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${incsrc}
            ${CMAKE_CURRENT_SOURCE_DIR}/common.fypp)
  add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${f90file}
    COMMAND fypp "${fyppFlags}" ${CMAKE_CURRENT_SOURCE_DIR}/${fyppfile}
            ${CMAKE_CURRENT_BINARY_DIR}/${f90file}
    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${incfile}
            ${CMAKE_CURRENT_SOURCE_DIR}/${fyppfile}
            ${CMAKE_CURRENT_SOURCE_DIR}/common.fypp)
  list(APPEND f90files ${CMAKE_CURRENT_BINARY_DIR}/${f90file})
endforeach()

add_custom_target(custom_header DEPENDS ${f90files})

set(unittest_src unittest.f90)
add_library(${PROJECT_NAME} STATIC ${unittest_src})
add_dependencies(${PROJECT_NAME} custom_header)
target_include_directories(${PROJECT_NAME} PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
set_target_properties(
  ${PROJECT_NAME} PROPERTIES CMAKE_POSITION_INDEPENDENT_CODE ON
                             WINDOWS_EXPORT_ALL_SYMBOLS ON)

set(LIB_MOD_DIR ${CMAKE_CURRENT_BINARY_DIR}/mod_files/)

if(NOT EXISTS "${LIB_MOD_DIR}")
  make_directory("${LIB_MOD_DIR}")
endif()

set_target_properties(${PROJECT_NAME} PROPERTIES Fortran_MODULE_DIRECTORY
                                                 ${LIB_MOD_DIR})
target_include_directories(
  ${PROJECT_NAME} PUBLIC $<BUILD_INTERFACE:${LIB_MOD_DIR}>
                         $<INSTALL_INTERFACE:${CMAKE_INSTALL_MODULEDIR}>)
target_compile_options(
  ${PROJECT_NAME}
  PRIVATE
    $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-O2>
    $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-g>
    $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-cpp>
    $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-std=f2008>
    $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-fimplicit-none>
    $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-fall-intrinsics>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-Wuninitialized>>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-fbounds-check>>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-Wunused>>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-fbacktrace>>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-fcheck=array-temps,bounds,do,mem,pointer>>
    $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-O2>
    $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-fpp>
    $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-stand>
    $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:f08>
    $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-free>
    $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-implicitnone>
    $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-qopt-matmul>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-warn>>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,Intel>:all>>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-check>>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,Intel>:bounds>>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-traceback>>)

if(CMAKE_BUILD_TYPE MATCHES "Debug")
  add_executable(test_unittest test_unittest.f90)
  target_include_directories(test_unittest PRIVATE ${LIB_MOD_DIR})
  target_link_libraries(test_unittest PRIVATE unittest)
  add_test(NAME test_unittest COMMAND test_unittest)
endif()

install(
  TARGETS unittest
  EXPORT unittest-export
  ARCHIVE DESTINATION "${CMAKE_INSTALL_PREFIX}/lib"
  LIBRARY DESTINATION "${CMAKE_INSTALL_PREFIX}/lib")

install(DIRECTORY ${LIB_MOD_DIR}
        DESTINATION "${CMAKE_INSTALL_PREFIX}/cmake/${PROJECT_NAME}/mod_files/")

install(
  EXPORT ${PROJECT_NAME}-export
  FILE unittest-config.cmake
  DESTINATION "${CMAKE_INSTALL_PREFIX}/cmake/${PROJECT_NAME}")
