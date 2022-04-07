cmake_minimum_required(VERSION 3.16)
project(rivet)

set(CMAKE_CXX_COMPILER "clang++-12")
set(CMAKE_CXX_STANDARD 11)

find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIRS})
link_directories(${LLVM_LIBRARY_DIRS})
add_definitions(${LLVM_DEFINITIONS})

find_program(LLVM_CONFIG_EXECUTABLE NAMES llvm-config)

execute_process(
	COMMAND ${LLVM_CONFIG_EXECUTABLE} --libs core analysis native bitwriter
	OUTPUT_VARIABLE LLVM_LIBRARIES
	OUTPUT_STRIP_TRAILING_WHITESPACE
)

include_directories(${PROJECT_SOURCE_DIR}/include)

file(GLOB_RECURSE SOURCES src/*.cpp)

file(GLOB SOURCES_WITHOUT_MAIN "${PROJECT_SOURCE_DIR}/src/*.cpp")
list(REMOVE_ITEM SOURCES_WITHOUT_MAIN "${PROJECT_SOURCE_DIR}/src/main.cpp")

add_executable(rivetc ${SOURCES})

add_library(rivetd STATIC ${SOURCES_WITHOUT_MAIN})
target_link_libraries(rivetd ${LLVM_LIBRARIES})
target_link_libraries(rivetc rivetd)
target_compile_options(rivetc PRIVATE -Wall -Wextra -Wmost -Weverything -pedantic -Werror -Wno-padded -Wno-c++98-compat)

# enable_testing()
# add_subdirectory(tests)
