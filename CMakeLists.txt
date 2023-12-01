# minimum version based on Ubuntu 22.04.03 LTS on 2023-11-19
cmake_minimum_required(VERSION 3.22)

# set the project name
project(alife VERSION 0.0 LANGUAGES Fortran)

# executables will be built and placed in bin/ directory
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)

# source code will be in src/ folder
add_subdirectory(src)
