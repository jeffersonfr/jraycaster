cmake_minimum_required (VERSION 3.0)

pkg_check_modules(JCANVAS jcanvas)

if (NOT JCANVAS_FOUND)
  message (SEND_ERROR "unable to find jcanvas library")
endif()

include_directories(
  ${CMAKE_SOURCE_DIR}/include
  ${JCANVAS_INCLUDE_DIRS}
)

add_executable(raycaster
    raycaster.cpp
  )

target_link_libraries(raycaster
  ${JCANVAS_LIBRARIES}
)

file(
  COPY ${CMAKE_CURRENT_SOURCE_DIR}/images 
  DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
  )

