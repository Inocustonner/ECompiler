add_executable(ec main.cpp
                  Lexer.cpp
				  Parser.cpp
                  error.cpp)
target_include_directories(ec PRIVATE ${CMAKE_SOURCE_DIR}/include)