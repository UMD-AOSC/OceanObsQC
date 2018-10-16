set(srcs
  prof_sort.F90
  set_real.F90
  running_stats.F90
  cubic_spline.f90
)


foreach(s ${srcs})
  target_sources(obsqc PRIVATE ${CMAKE_CURRENT_LIST_DIR}/${s})
endforeach()
