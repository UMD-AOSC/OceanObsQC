find_library(BUFR_LIBRARY NAMES bufr BUFR bufrlib)
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Bufr DEFAULT_MSG 
  BUFR_LIBRARY)
