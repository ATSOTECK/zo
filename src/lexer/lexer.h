#pragma once

#include "common/diagnostics.h"
#include "lexer/token.h"

#include <string>
#include <vector>

namespace zo {

class Lexer {
public:
    Lexer(const std::string& source, const std::string& filename, DiagnosticEngine& diag);

    std::vector<Token> tokenize();

private:
    Token nextToken();
    Token makeToken(TokenKind kind, const std::string& text);
    Token makeToken(TokenKind kind);

    char peek() const;
    char peekNext() const;
    char advance();
    bool match(char expected);
    bool isAtEnd() const;

    void skipWhitespace();
    void skipLineComment();
    void skipBlockComment();

    Token lexString();
    Token lexRune();
    Token lexNumber();
    Token lexIdentOrKeyword();

    bool shouldInsertSemicolon(TokenKind lastKind) const;

    std::string source_;
    std::string filename_;
    DiagnosticEngine& diag_;

    size_t pos_ = 0;
    uint32_t line_ = 1;
    uint32_t col_ = 1;
};

} // namespace zo
