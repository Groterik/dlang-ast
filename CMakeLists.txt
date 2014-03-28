cmake_minimum_required(VERSION 2.6)

#Project name
set(PROJECT dlang-ast)

enable_testing()

project(${PROJECT})

find_package(BISON REQUIRED)

find_package(FLEX REQUIRED)

set(CXX11_FLAG "-std=c++11")
if(CMAKE_COMPILER_IS_GNUCC)
    if(CMAKE_CXX_COMPILER_VERSION VERSION_LESS 4.7)
      set(CXX11_FLAG "-std=c++0x")
    endif()
endif()

set(PUBLIC_HEADERS
      dast.h)

set(PRIVATE_HEADERS)

set(LEX_FILE dlang.l)
set(GRM_FILE dlang.y)

set(LEX_SOURCES ${CMAKE_CURRENT_BINARY_DIR}/dlang_lex.cpp)
set(GRM_SOURCES ${CMAKE_CURRENT_BINARY_DIR}/dlang_grm.cpp)

FLEX_TARGET(FLEX_OUT ${LEX_FILE} ${LEX_SOURCES})
BISON_TARGET(BISON_OUT ${GRM_FILE} ${GRM_SOURCES})

set(SOURCES
    dast.cpp
    main.cpp
    ${LEX_SOURCES}
    ${GRM_SOURCES})

include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(${CMAKE_CURRENT_BINARY_DIR})

set(CXX11_FLAG "-std=c++11")
if(CMAKE_COMPILER_IS_GNUCC)
    if(CMAKE_CXX_COMPILER_VERSION VERSION_LESS 4.7)
      set(CXX11_FLAG "-std=c++0x")
    endif()
endif()

# Target building and properties
add_executable(${PROJECT} ${SOURCES} ${PUBLIC_HEADERS} ${PRIVATE_HEADERS} ${LEX_FILE} ${GRM_FILE})
set_property(TARGET ${PROJECT} PROPERTY PUBLIC_HEADER ${PUBLIC_HEADERS})
set_property(TARGET ${PROJECT} APPEND PROPERTY COMPILE_FLAGS ${CXX11_FLAG})
install(TARGETS ${PROJECT}
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
        PUBLIC_HEADER DESTINATION include)

