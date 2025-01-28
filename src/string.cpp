#include "string.hpp"
#include <list>

static std::list<std::string> strings;

char* StringManager::create(const char* str, u32 len) {
    for (auto& s : strings) {
        if (s.size() != len) continue;
        if (memcmp(s.data(), str, len) == 0)
            return s.data();
    }
    strings.push_back(std::string(str, len));
    return strings.back().data();
}