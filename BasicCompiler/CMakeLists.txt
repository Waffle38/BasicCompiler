# CMakeList.txt : CMake project for BasicCompiler, include source and define
# project specific logic here.
#
cmake_minimum_required (VERSION 3.20)
set(CMAKE_CXX_STANDARD 17)

set(BUILD_ARCH "-m64")
set(TARGET_64 ON)

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Add source to this project's executable.
add_executable (BasicCompiler "BasicCompiler.cpp" "../include/KaleidoscopeJIT.h")

llvm_map_components_to_libnames(llvm_libs
	Core
	Support
	OrcJIT
	native
	)


target_link_libraries(BasicCompiler ${llvm_libs})