cmake_minimum_required(VERSION 3.30)
project(zo)

set(CMAKE_CXX_STANDARD 20)

find_package(libgit2 REQUIRED)

if(TARGET libgit2::libgit2)
    set(LIBGIT2_TARGET libgit2::libgit2)
elseif(TARGET libgit2::libgit2package)
    set(LIBGIT2_TARGET libgit2::libgit2package)
endif()

include_directories(${PROJECT_SOURCE_DIR}/src)
include_directories(${PROJECT_SOURCE_DIR}/inc)
link_directories(${PROJECT_SOURCE_DIR}/lib)

add_executable(zo
    src/main.cpp
    src/common/diagnostics.cpp
    src/lexer/token.cpp
    src/lexer/lexer.cpp
    src/parser/parser.cpp
    src/codegen/go_codegen.cpp
    src/sema/symbol_table.cpp
    src/sema/type_checker.cpp
    src/driver/compiler.cpp
    src/driver/project.cpp
    src/pkg/semver.cpp
    src/pkg/build_config.cpp
    src/pkg/lock_file.cpp
    src/pkg/git_ops.cpp
    src/pkg/resolver.cpp
)

target_link_libraries(zo ${LIBGIT2_TARGET})

if(APPLE)
    target_link_libraries(zo "-framework CoreFoundation")
    target_link_libraries(zo "-framework Security")
    set_target_properties(zo PROPERTIES LINK_FLAGS "-Wl,-F/Library/Frameworks")
    target_link_libraries(zo libiconv.dylib)
    target_link_libraries(zo libz.dylib)
endif()
