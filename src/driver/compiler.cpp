#include "driver/compiler.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "sema/type_checker.h"
#include "codegen/go_codegen.h"

namespace zo {

Compiler::Compiler(DiagnosticEngine& diag) : diag_(diag) {}

std::string Compiler::compile(const std::string& source, const std::string& filename) {
    // Lex
    Lexer lexer(source, filename, diag_);
    auto tokens = lexer.tokenize();
    if (diag_.hasErrors()) return "";

    // Parse
    Parser parser(std::move(tokens), diag_);
    auto file = parser.parseFile();
    if (diag_.hasErrors()) return "";

    // Type check
    if (checkTypes_) {
        TypeChecker checker(diag_);
        checker.check(file);
        if (diag_.hasErrors()) return "";
    }

    // Codegen
    GoCodegen codegen(diag_);
    return codegen.generate(file);
}

File Compiler::parse(const std::string& source, const std::string& filename) {
    Lexer lexer(source, filename, diag_);
    auto tokens = lexer.tokenize();
    Parser parser(std::move(tokens), diag_);
    return parser.parseFile();
}

} // namespace zo
