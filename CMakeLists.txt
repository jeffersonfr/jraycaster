cmake_minimum_required (VERSION 3.0)

if (CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
  message(FATAL_ERROR "In-source build is not allowed")
endif()

project(jraycaster
  LANGUAGES CXX
  HOMEPAGE_URL https://github.com/jeffersonfr/jraycaster
  DESCRIPTION "Raycaster sample"
  VERSION 1.0.0
)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(PkgConfig REQUIRED)

enable_testing()

add_subdirectory(src)
