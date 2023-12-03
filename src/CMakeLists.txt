# explicitly name all files in src/ directory
set(MODULE_FILES 
    day_01.f90
    day_02.f90
    day_03.f90
   )

# compile source files to lib_modules
add_library(lib_modules STATIC ${MODULE_FILES})

# compile tests executables
add_executable(aoc2023 main.f90)
target_link_libraries(aoc2023 lib_modules)
