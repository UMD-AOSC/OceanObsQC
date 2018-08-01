cmake_minimum_required(VERSION 3.0 FATAL_ERROR)

project(OceanObsQC LANGUAGES Fortran)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)

# compile 3rd party dependencies
add_subdirectory(dep)

# compile our code
add_subdirectory(src)
