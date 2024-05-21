cmake_minimum_required( VERSION 3.14 )
project( unittest
  DESCRIPTION "Fortran90 unittest"
  LANGUAGES Fortran
  )

cmake_policy( SET CMP0076 NEW )

enable_testing()
include( CTest )

include( CMakePrintHelpers )
enable_language( Fortran )

add_subdirectory( src )

