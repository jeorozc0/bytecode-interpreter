cmake_minimum_required(VERSION 3.30)
project(Craftin_Interpreters C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_executable(Craftin_Interpreters main.c
        common.h
        chunk.h
        chunk.c
        memory.h
        memory.c
        debug.h
        debug.c
        value.h
        value.c
        vm.h
        vm.c
        compiler.h
        compiler.c
        scanner.h
        scanner.c
        object.h
        object.c
        table.c
        table.h)
