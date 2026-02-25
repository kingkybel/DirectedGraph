cmake_minimum_required(VERSION 3.26)
project(DirectedGraph
        VERSION 1.0.0
        DESCRIPTION "A header only library for directed graphs"
        HOMEPAGE_URL "https://github.com/kingkybel/DirectedGraphs")

include(GNUInstallDirs)

set(DKYB_CMAKE_COMMON_LOCAL_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake-common" CACHE PATH "Path to local dkyb cmake-common checkout")
set(DKYB_CMAKE_COMMON_GIT_REPOSITORY "https://github.com/kingkybel/cmake-common.git" CACHE STRING "dkyb cmake-common repository")
set(DKYB_CMAKE_COMMON_GIT_TAG "v0.1.0" CACHE STRING "Pinned dkyb cmake-common tag")

set(_dkyb_common_module "${DKYB_CMAKE_COMMON_LOCAL_PATH}/cmake/DkybBuildSettings.cmake")
if (EXISTS "${_dkyb_common_module}")
    include("${_dkyb_common_module}")
else ()
    include(FetchContent)
    FetchContent_Declare(
            dkyb_cmake_common
            GIT_REPOSITORY "${DKYB_CMAKE_COMMON_GIT_REPOSITORY}"
            GIT_TAG "${DKYB_CMAKE_COMMON_GIT_TAG}"
    )
    FetchContent_MakeAvailable(dkyb_cmake_common)
    include("${dkyb_cmake_common_SOURCE_DIR}/cmake/DkybBuildSettings.cmake")
endif ()

dkyb_apply_common_settings(
        EXTRA_CXX_FLAGS
        -DBOOST_ALLOW_DEPRECATED_HEADERS
        -DBOOST_BIND_GLOBAL_PLACEHOLDERS
        -Wno-error=maybe-uninitialized
)

set(CMAKE_FIND_LIBRARY_PREFIXES lib)
set(CMAKE_FIND_LIBRARY_SUFFIXES ".a")
set(BUILD_SHARED_LIBS OFF)
set(CMAKE_EXE_LINKER_FLAGS "-static")

# include(FetchContent)
# FetchContent_Declare(
# googletest
# # Specify the commit you depend on and update it regularly.
# URL https://github.com/google/googletest/archive/5376968f6948923e2411081fd9372e71a59d8e77.zip
# )
# set(FETCHCONTENT_QUIET OFF)
# For Windows: Prevent overriding the parent project's compiler/linker settings
# set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
# FetchContent_MakeAvailable(googletest)

# include(FetchContent)
# FetchContent_Declare(
# Boost
# GIT_REPOSITORY https://github.com/boostorg/boost.git
# GIT_TAG boost-1.83.0
# )

find_package(Boost 1.90.0 REQUIRED)
message("Boost_INCLUDE_DIRS '${Boost_INCLUDE_DIRS}'")

include_directories(
        ${Boost_INCLUDE_DIRS}
        BEFORE SYSTEM ${CMAKE_SOURCE_DIR}/include
        system
)
link_directories(
        /usr/local/lib
        ${Boost_LIBRARY_DIR_RELEASE}
        system
)

add_library(DirectedGraphs INTERFACE)
# Adding the install interface generator expression makes sure that the include
# files are installed to the proper location (provided by GNUInstallDirs)
target_include_directories(
        DirectedGraphs
        INTERFACE $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)

target_compile_features(DirectedGraphs INTERFACE cxx_std_23)

add_subdirectory(test)

set(CMAKE_CXX_CLANG_TIDY "clang-tidy;-checks=* -p ${CMAKE_SOURCE_DIR}/build")

file(GLOB HEADER_FILES ${CMAKE_SOURCE_DIR}/include/*.h)

# Install the header files directly under 'include/dkyb'
install(FILES ${HEADER_FILES}
        DESTINATION include/dkyb)
