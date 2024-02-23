# - Find libev
# Find the native LLVM includes and library
#
#  LLVM_INCLUDE_DIR - where to find ev.h, etc.
#  LLVM_LIBRARIES   - List of libraries when using libev.
#  LLVM_FOUND       - True if libev found.

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

llvm_map_components_to_libnames(LLVM_ALL_LIBS
        asmprinter
        irprinter
        passes
        support
        core
        executionengine
        mcjit
        orcjit
)

foreach(target ${LLVM_TARGETS_TO_BUILD})
    if (NOT "${target}" MATCHES "^(NVPTX|XCore)")
        list(APPEND LLVM_ALL_LIBS "LLVM${target}AsmParser")
    endif()
    list(APPEND LLVM_ALL_LIBS "LLVM${target}CodeGen")
endforeach()



#find_program(LLVM_CONFIG_EXECUTABLE NAMES "${LLVM_PATH}/bin/llvm-config" DOC "llvm-config executable")
#
#execute_process(
#	COMMAND ${LLVM_CONFIG_EXECUTABLE} --cppflags
#	OUTPUT_VARIABLE LLVM_CFLAGS
#	OUTPUT_STRIP_TRAILING_WHITESPACE
#)
#
#execute_process(
#	COMMAND ${LLVM_CONFIG_EXECUTABLE} --ldflags
#	OUTPUT_VARIABLE LLVM_LFLAGS
#	OUTPUT_STRIP_TRAILING_WHITESPACE
#)
#execute_process(
#	COMMAND ${LLVM_CONFIG_EXECUTABLE} --libs core jit native linker bitreader bitwriter ipo
#	OUTPUT_VARIABLE LLVM_JIT_LIBS
#	OUTPUT_STRIP_TRAILING_WHITESPACE
#)
#execute_process(
#	COMMAND ${LLVM_CONFIG_EXECUTABLE} --libs all
#	OUTPUT_VARIABLE LLVM_ALL_LIBS
#	OUTPUT_STRIP_TRAILING_WHITESPACE
#)
#
