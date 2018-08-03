# find the NCEP BUFRLIB
find_package(Bufr REQUIRED)

# include the "fortran template library"
include_directories(${GFTL_INCLUDE})

# find netcdf
set(NETCDF_F90 "YES")
find_package(NetCDF REQUIRED)
include_directories(${NETCDF_INCLUDE_DIRS})

include_directories(${GSW_INCLUDES})


# core source files and exectubable creation
set(obsqc_srcs
  obsqc.F90
  obs_reader.F90
  obs_writer.F90
  profile.F90
  qc_step.F90
)

add_executable( obsqc ${obsqc_srcs})
target_link_libraries(obsqc
  ${BUFR_LIBRARY}
  gsw
  ${NETCDF_LIBRARIES}
)

# add plugin source files to the executable
add_subdirectory(obs_readers)
add_subdirectory(obs_writers)
add_subdirectory(qc_steps)
