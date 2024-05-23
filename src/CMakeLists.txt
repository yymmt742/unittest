set(LIB_MOD_DIR ${CMAKE_CURRENT_BINARY_DIR}/mod_files/)
if(NOT EXISTS "${LIB_MOD_DIR}")
  make_directory("${LIB_MOD_DIR}")
endif()

check_fortran_source_runs(
  "i=0; error stop i; end"
  f18errorstop
  )

function(fypp_generate src target opts list newlist)
  add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${target}
    COMMAND fypp ${opts}
            ${CMAKE_CURRENT_SOURCE_DIR}/${src}
            ${CMAKE_CURRENT_BINARY_DIR}/${target}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${src}
            ${CMAKE_CURRENT_SOURCE_DIR}/common.fypp
    )
  set(list_)
  foreach(t IN LISTS list)
    list(APPEND list_ ${t})
  endforeach()
  list(APPEND list_ ${CMAKE_CURRENT_BINARY_DIR}/${target})
  set(${newlist} ${list_} PARENT_SCOPE)
endfunction()

set(f90files)
set(incfiles)

if(f18errorstop)
  fypp_generate("error_stop_f18.fypp" "error_stop.f90" "${fyppFlags}" "${f90files}" f90files)
else()
  fypp_generate("error_stop_f08.fypp" "error_stop.f90" "${fyppFlags}" "${f90files}" f90files)
endif()

fypp_generate("mod_kinds.fypp" "mod_kinds.f90" "${fyppFlags}" "${f90files}" f90files)

list(APPEND fyppFiles
   bool.fypp
   equal.fypp
   almost_equal.fypp
   compare.fypp
   )

foreach(fyppFile ${fyppFiles})
  string(REGEX REPLACE "\\.fypp" ".f90" f90Target ${fyppFile})
  string(REGEX REPLACE "\\.fypp" ".h.fypp" hFile ${fyppFile})
  string(REGEX REPLACE "\\.fypp" ".h" hTarget ${fyppFile})
  string(REGEX REPLACE "\\.fypp" ".i.fypp" incFile ${fyppFile})
  string(REGEX REPLACE "\\.fypp" ".inc" incTarget ${fyppFile})
  fypp_generate("${hFile}" "${hTarget}" "${fyppFlags}" "${incfiles}" incfiles)
  fypp_generate("${incFile}" "${incTarget}" "${fyppFlags}" "${incfiles}" incfiles)
  fypp_generate("${fyppFile}" "${f90Target}" "${fyppFlags}" "${f90files}" f90files)
endforeach()

add_custom_target(custom_header SOURCES ${incfiles})

add_library(
  ${PROJECT_NAME} STATIC
  ${f90files}
  ${CMAKE_CURRENT_SOURCE_DIR}/mod_unittest_printer.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/mod_unittest.f90
  )
add_dependencies(${PROJECT_NAME} custom_header)
target_include_directories(
  ${PROJECT_NAME} PRIVATE
  ${CMAKE_CURRENT_BINARY_DIR}
  ${LIB_MOD_DIR}
  )
set_target_properties(
  ${PROJECT_NAME} PROPERTIES
  CMAKE_POSITION_INDEPENDENT_CODE ON
  WINDOWS_EXPORT_ALL_SYMBOLS ON
  )
set_target_properties(
  ${PROJECT_NAME} PROPERTIES
  Fortran_MODULE_DIRECTORY
  ${LIB_MOD_DIR}
  )
target_include_directories(
  ${PROJECT_NAME} PUBLIC
  $<BUILD_INTERFACE:${LIB_MOD_DIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_MODULEDIR}>
  )
target_compile_options(
  ${PROJECT_NAME}
  PRIVATE
    $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-O2>
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
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-traceback>>
    )

install(
  TARGETS unittest
  EXPORT unittest-export
  ARCHIVE DESTINATION "${CMAKE_INSTALL_PREFIX}/lib"
  LIBRARY DESTINATION "${CMAKE_INSTALL_PREFIX}/lib"
  )
install(
  DIRECTORY ${LIB_MOD_DIR}
  DESTINATION "${CMAKE_INSTALL_PREFIX}/cmake/${PROJECT_NAME}/mod_files/"
  )
install(
  EXPORT ${PROJECT_NAME}-export
  FILE unittest-config.cmake
  DESTINATION "${CMAKE_INSTALL_PREFIX}/cmake/${PROJECT_NAME}"
  )
