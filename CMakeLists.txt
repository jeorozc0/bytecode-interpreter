cmake_minimum_required(VERSION 3.30)
project(Craftin_Interpreters C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Add the include directory for header files.
include_directories(${CMAKE_SOURCE_DIR}/include)

# List source files with their new paths.
set(SOURCES
    src/main.c
    src/chunk.c
    src/compiler.c
    src/debug.c
    src/memory.c
    src/object.c
    src/scanner.c
    src/table.c
    src/value.c
    src/vm.c
)

add_executable(Craftin_Interpreters ${SOURCES})

