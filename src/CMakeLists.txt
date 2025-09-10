cmake_minimum_required (VERSION 3.0)

pkg_check_modules(jCanvas REQUIRED IMPORTED_TARGET jcanvas)

add_executable(raycaster
    raycaster.cpp
  )

target_link_libraries(raycaster
  PRIVATE
    PkgConfig::jCanvas
		cairo
  )

file(
  COPY ${CMAKE_CURRENT_SOURCE_DIR}/images 
  DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
  )

