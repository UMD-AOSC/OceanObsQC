# "fortran template library"
add_subdirectory(gFTL)
set(GFTL_INCLUDE
  ${CMAKE_CURRENT_BINARY_DIR}/gFTL/include
  ${CMAKE_CURRENT_LIST_DIR}/gFTL/include
  PARENT_SCOPE)

# Gibbs Seawater
#TODO, need to do this properly so that mod files are generated in their own subdirectory
include(GSW-Fortran.cmake)
set(GSW_INCLUDES ${GSW_INCLUDES} PARENT_SCOPE)
