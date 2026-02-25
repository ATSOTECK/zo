#include "common/diagnostics.h"

namespace zo {

void DiagnosticEngine::error(const SourceLocation& loc, const std::string& msg) {
    emit(DiagLevel::Error, loc, msg);
}

void DiagnosticEngine::warning(const SourceLocation& loc, const std::string& msg) {
    emit(DiagLevel::Warning, loc, msg);
}

void DiagnosticEngine::note(const SourceLocation& loc, const std::string& msg) {
    emit(DiagLevel::Note, loc, msg);
}

void DiagnosticEngine::emit(DiagLevel level, const SourceLocation& loc, const std::string& msg) {
    diags_.push_back({level, loc, msg});
    if (level == DiagLevel::Error) {
        ++error_count_;
    }
    print(diags_.back());
}

void DiagnosticEngine::print(const Diagnostic& diag) const {
    const char* color = "";
    const char* label = "";
    const char* reset = "\033[0m";

    switch (diag.level) {
        case DiagLevel::Error:
            color = "\033[1;31m";
            label = "error";
            break;
        case DiagLevel::Warning:
            color = "\033[1;33m";
            label = "warning";
            break;
        case DiagLevel::Note:
            color = "\033[1;36m";
            label = "note";
            break;
    }

    std::cerr << "\033[1m" << diag.loc.str() << ": "
              << color << label << ": " << reset
              << "\033[1m" << diag.message << reset << "\n";
}

void DiagnosticEngine::printAll() const {
    for (const auto& diag : diags_) {
        print(diag);
    }
}

} // namespace zo
