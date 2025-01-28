#pragma once

#include <string>
#include "common.hpp"

void print_error(u32 line, u32 column, char* filename, const std::string& msg);