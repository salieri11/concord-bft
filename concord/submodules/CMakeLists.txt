set(BUILD_TESTING OFF CACHE BOOL "tests not needed")
add_subdirectory(concord-bft)
unset(BUILD_TESTING)

set(HDR_HISTOGRAM_BUILD_PROGRAMS OFF CACHE BOOL "programs not needed")
set(HDR_HISTOGRAM_BUILD_SHARED OFF CACHE BOOL "we link statically")
add_subdirectory(HdrHistogram_c)
