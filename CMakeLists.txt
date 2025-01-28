cmake_minimum_required(VERSION 3.30.3)
project(cannon)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(LLVM REQUIRED CONFIG)

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

file(GLOB_RECURSE SOURCES
    src/*.cpp
)

set(CMAKE_BUILD_TYPE Debug)

add_executable(cannon ${SOURCES})

llvm_map_components_to_libnames(llvm_libs support core irreader)
target_link_libraries(cannon ${llvm_libs})
