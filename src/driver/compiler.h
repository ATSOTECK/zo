#pragma once

#include "common/diagnostics.h"
#include "parser/ast.h"

#include <string>

namespace zo {

class Compiler {
public:
    explicit Compiler(DiagnosticEngine& diag);

    // Compile a .zo source file to Go source code.
    // Returns the generated Go code, or empty string on error.
    std::string compile(const std::string& source, const std::string& filename);

    // Compile and return the parsed AST (for testing/tooling).
    File parse(const std::string& source, const std::string& filename);

    void setCheckTypes(bool enabled) { checkTypes_ = enabled; }

private:
    DiagnosticEngine& diag_;
    bool checkTypes_ = true;
};

} // namespace zo
