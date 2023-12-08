# explicitly name all files in src/ directory
set(MODULE_FILES 
    string_utils.f90
    day_01.f90
    day_02.f90
    day_03.f90
    day_04.f90
    day_05.f90
    day_06.f90
    day_07.f90
   )

# compile source files to lib_modules
add_library(lib_modules STATIC ${MODULE_FILES})

# compile executables
add_executable(aoc2023 main.f90)
target_link_libraries(aoc2023 lib_modules)

add_executable(test_string_utils test_string_utils.f90)
target_link_libraries(test_string_utils lib_modules)
