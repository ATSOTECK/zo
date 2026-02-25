#pragma once

#include "parser/ast.h"

#include <string>

namespace zo {

class CodeGenerator {
public:
    virtual ~CodeGenerator() = default;
    virtual std::string generate(const File& file) = 0;
};

} // namespace zo
