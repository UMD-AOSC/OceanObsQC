find_package(Bufr REQUIRED)

# include the "fortran template library"
include_directories(${FTL_INCLUDE})

# core source files and exectubable creation
set(obsqc_srcs
  obsqc.F90
  obs_reader.F90
  obs_writer.F90
  profile.F90
  qc_step.F90
)

add_executable( obsqc ${obsqc_srcs})
target_link_libraries(obsqc ${BUFR_LIBRARY} gsw)

# add plugin source files to the executable
add_subdirectory(obs_readers)
add_subdirectory(obs_writers)
add_subdirectory(qc_steps)
