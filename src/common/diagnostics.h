#pragma once

#include "common/source_location.h"

#include <string>
#include <vector>
#include <iostream>

namespace zo {

enum class DiagLevel {
    Error,
    Warning,
    Note,
};

struct Diagnostic {
    DiagLevel level;
    SourceLocation loc;
    std::string message;
};

class DiagnosticEngine {
public:
    void error(const SourceLocation& loc, const std::string& msg);
    void warning(const SourceLocation& loc, const std::string& msg);
    void note(const SourceLocation& loc, const std::string& msg);

    void emit(DiagLevel level, const SourceLocation& loc, const std::string& msg);
    void print(const Diagnostic& diag) const;
    void printAll() const;

    bool hasErrors() const { return error_count_ > 0; }
    int errorCount() const { return error_count_; }
    const std::vector<Diagnostic>& diagnostics() const { return diags_; }

private:
    std::vector<Diagnostic> diags_;
    int error_count_ = 0;
};

} // namespace zo
