#pragma once

#include <string>
#include "common.hpp"

class StringManager {
public:
    static char* create(const char* str, u32 len);

    inline static char* create(const char* str) { return create(str, strlen(str)); }
};