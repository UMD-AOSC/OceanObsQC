# "fortran template library"
add_subdirectory(gFTL)
set(GFTL_INCLUDE ${CMAKE_CURRENT_BINARY_DIR}/gFTL/include;${CMAKE_CURRENT_LIST_DIR}/gFTL/include PARENT_SCOPE)

# Gibbs Seawater
include(GSW-Fortran.cmake)

#add_subdirectory(bufrlib)
