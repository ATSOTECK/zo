#pragma once

#include <cstdint>
#include <string>

namespace zo {

struct SourceLocation {
    std::string file;
    uint32_t line = 1;
    uint32_t col = 1;
    uint32_t offset = 0;

    std::string str() const {
        return file + ":" + std::to_string(line) + ":" + std::to_string(col);
    }
};

struct SourceSpan {
    SourceLocation start;
    SourceLocation end;
};

} // namespace zo
