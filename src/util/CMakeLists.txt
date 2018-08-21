set(UTIL_SRCS
  ${CMAKE_CURRENT_LIST_DIR}/prof_sort.F90
)

target_sources(obsqc PRIVATE ${UTIL_SRCS})
