cmake_minimum_required (VERSION 3.0)

project (jraycaster)

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -Wall")

find_package(PkgConfig REQUIRED)

pkg_check_modules(jlibcpp REQUIRED jlibcpp)

add_subdirectory(src)
