set(LIB_MOD_DIR ${CMAKE_CURRENT_BINARY_DIR}/mod_files/)
if(NOT EXISTS "${LIB_MOD_DIR}")
  make_directory("${LIB_MOD_DIR}")
endif()

check_fortran_source_runs(
  "i=0; error stop i; end"
  f18errorstop
  )

function(fypp_generate src target opts depends list newlist)
  add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${target}
    COMMAND fypp ${opts}
            ${CMAKE_CURRENT_SOURCE_DIR}/${src}
            ${CMAKE_CURRENT_BINARY_DIR}/${target}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${src}
            ${CMAKE_CURRENT_SOURCE_DIR}/../include/common.fypp
            ${depends}
    )
  set(list_)
  foreach(t IN LISTS list)
    list(APPEND list_ ${t})
  endforeach()
  list(APPEND list_ ${CMAKE_CURRENT_BINARY_DIR}/${target})
  set(${newlist} ${list_} PARENT_SCOPE)
endfunction()

list(APPEND fyppIncludes
   result_functions.fypp
   image_functions.fypp
   )

list(APPEND fyppSources
   mod_unittest_kinds.fypp
   mod_unittest_tap14_yaml.fypp
   bool.fypp
   equal.fypp
   almost_equal.fypp
   compare.fypp
   allclose.fypp
   is_eye.fypp
   is_zero.fypp
   is_symmetric.fypp
   mod_unittest.fypp
   )

set(f90files)
set(incfiles)

add_executable(
  get_constants
  ${CMAKE_CURRENT_SOURCE_DIR}/get_constants.f90
  )
add_custom_command(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/constants.f90
  COMMAND get_constants
          ${CMAKE_CURRENT_BINARY_DIR}/constants.f90
  )

list(APPEND incfiles ${CMAKE_CURRENT_BINARY_DIR}/constants.f90)
list(APPEND incfiles ${CMAKE_CURRENT_SOURCE_DIR}/report_functions.fypp)

if(f18errorstop)
  fypp_generate("error_stop_f18.fypp" "error_stop.f90" "${fyppFlags}" "" "${f90files}" f90files)
else()
  fypp_generate("error_stop_f08.fypp" "error_stop.f90" "${fyppFlags}" "" "${f90files}" f90files)
endif()
foreach(fyppFile ${fyppIncludes})
  string(REGEX REPLACE "\\.fypp" ".f90" f90Target ${fyppFile})
  fypp_generate("${fyppFile}" "${f90Target}" "${fyppFlags}" "" "${incfiles}" incfiles)
endforeach()
foreach(fyppFile ${fyppSources})
  string(REGEX REPLACE "\\.fypp" ".f90" f90Target ${fyppFile})
  fypp_generate("${fyppFile}" "${f90Target}" "${fyppFlags}" "${incfiles}" "${f90files}" f90files)
endforeach()

add_library(
  ${PROJECT_NAME} STATIC
  ${f90files}
  )
target_include_directories(
  ${PROJECT_NAME} PRIVATE
  ${CMAKE_CURRENT_BINARY_DIR}
  )
target_include_directories(
  ${PROJECT_NAME} PUBLIC
  $<BUILD_INTERFACE:${LIB_MOD_DIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_MODULEDIR}>
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
target_compile_options(
  ${PROJECT_NAME}
  PRIVATE
    $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-O2>
    $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-std=f2008>
    $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-fimplicit-none>
    $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-fall-intrinsics>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-Wuninitialized>>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-fbounds-check>>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-Wunused>>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-fbacktrace>>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-fcheck=array-temps,bounds,do,mem,pointer>>
    $<$<COMPILE_LANG_AND_ID:Fortran,NVHPC>:-fast>
    $<$<COMPILE_LANG_AND_ID:Fortran,NVHPC>:-Mnobackslash>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,NVHPC>:-C>>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,NVHPC>:-g>>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,NVHPC>:-traceback>>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,NVHPC>:-Wall>>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,NVHPC>:-Mbounds>>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,NVHPC>:-Mchkptr>>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,NVHPC>:-Mchkstk>>
    )
 if(UNIX)
   target_compile_options(
   ${PROJECT_NAME}
   PRIVATE
     $<$<COMPILE_LANG_AND_ID:Fortran,IntelLLVM>:-free>
     $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,IntelLLVM>:-g>>
     $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,IntelLLVM>:-traceback>>
     $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,IntelLLVM>:-debug>>
     $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,IntelLLVM>:-warn all>>
     $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-free>
     $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-g>>
     $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-traceback>>
     $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-debug>>
     $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-warn all>>
     )
elseif(WIN32)
  target_compile_options(
  ${PROJECT_NAME}
  PRIVATE
    $<$<COMPILE_LANG_AND_ID:Fortran,IntelLLVM>:/free>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,IntelLLVM>:/traceback>>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,IntelLLVM>:/debug:full>>
    $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:/free>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,Intel>:/traceback>>
    $<$<CONFIG:Debug>:$<$<COMPILE_LANG_AND_ID:Fortran,Intel>:/debug:full>>
    )
 endif()

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
