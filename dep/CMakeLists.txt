# "fortran template library"
add_subdirectory(ftl)
set(FTL_INCLUDE ${FTL_INCLUDE} PARENT_SCOPE)

# Gibbs Seawater
include(GSW-Fortran.cmake)

#add_subdirectory(bufrlib)
