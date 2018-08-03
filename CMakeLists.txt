cmake_minimum_required(VERSION 3.0 FATAL_ERROR)

project(OceanObsQC LANGUAGES Fortran)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)

# compile 3rd party dependencies
add_subdirectory(dep)

# determine the repository version
execute_process(COMMAND git describe --dirty --tags --always
  OUTPUT_VARIABLE GIT_VERSION
  RESULT_VARIABLE RES
  OUTPUT_STRIP_TRAILING_WHITESPACE
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
if(NOT ${RES} EQUAL "0")
  set(GIT_VERSION "Unknown")
endif()
add_definitions(-DCVERSION=\"${GIT_VERSION}\")
message(STATUS "Git repository version: ${GIT_VERSION}")

# compile our code
add_subdirectory(src)
