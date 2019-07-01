cmake_minimum_required (VERSION 3.0)

project (jraycaster)

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")

find_package(PkgConfig REQUIRED)

if (PKG_CONFIG_FOUND)
  pkg_check_modules(JLIBCPP REQUIRED jlibcpp)
endif()

add_subdirectory(src)
