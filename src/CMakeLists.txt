cmake_minimum_required (VERSION 3.0)

include_directories(
  ${JLIBCPP_INCLUDE_DIRS}
)

add_executable(raycaster
    raycaster.cpp
  )

target_link_libraries(raycaster
  ${JLIBCPP_LIBRARIES}
  )

add_definitions(${JLIBCPP_CFLAGS})

file(
  COPY ${CMAKE_CURRENT_SOURCE_DIR}/images 
  DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
  )
