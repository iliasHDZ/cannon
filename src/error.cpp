#include "error.hpp"
#include <iostream>

void print_error(u32 line, u32 column, char* filename, const std::string& msg) {
    std::cout << filename << ':' << (line + 1) << ':' << (column + 1) << ": " << msg << std::endl;
}