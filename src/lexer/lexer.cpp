#include "lexer/lexer.h"

#include <cctype>

namespace zo {

Lexer::Lexer(const std::string& source, const std::string& filename, DiagnosticEngine& diag)
    : source_(source), filename_(filename), diag_(diag) {}

char Lexer::peek() const {
    if (isAtEnd()) return '\0';
    return source_[pos_];
}

char Lexer::peekNext() const {
    if (pos_ + 1 >= source_.size()) return '\0';
    return source_[pos_ + 1];
}

char Lexer::advance() {
    char c = source_[pos_++];
    if (c == '\n') {
        ++line_;
        col_ = 1;
    } else {
        ++col_;
    }
    return c;
}

bool Lexer::match(char expected) {
    if (isAtEnd() || source_[pos_] != expected) return false;
    advance();
    return true;
}

bool Lexer::isAtEnd() const {
    return pos_ >= source_.size();
}

Token Lexer::makeToken(TokenKind kind, const std::string& text) {
    return Token{kind, text, {filename_, line_, col_, static_cast<uint32_t>(pos_)}};
}

Token Lexer::makeToken(TokenKind kind) {
    return makeToken(kind, std::string(1, source_[pos_ - 1]));
}

void Lexer::skipWhitespace() {
    while (!isAtEnd()) {
        char c = peek();
        if (c == ' ' || c == '\t' || c == '\r') {
            advance();
        } else {
            break;
        }
    }
}

void Lexer::skipLineComment() {
    while (!isAtEnd() && peek() != '\n') {
        advance();
    }
}

void Lexer::skipBlockComment() {
    // Already consumed /*
    int depth = 1;
    while (!isAtEnd() && depth > 0) {
        if (peek() == '/' && peekNext() == '*') {
            advance(); advance();
            ++depth;
        } else if (peek() == '*' && peekNext() == '/') {
            advance(); advance();
            --depth;
        } else {
            advance();
        }
    }
    if (depth > 0) {
        diag_.error({filename_, line_, col_}, "unterminated block comment");
    }
}

Token Lexer::lexString() {
    // Opening " already consumed
    uint32_t startLine = line_;
    uint32_t startCol = col_ - 1;
    std::string value;

    while (!isAtEnd() && peek() != '"') {
        if (peek() == '\n') {
            diag_.error({filename_, line_, col_}, "unterminated string literal");
            break;
        }
        if (peek() == '\\') {
            advance();
            if (isAtEnd()) break;
            char esc = advance();
            switch (esc) {
                case 'n':  value += '\n'; break;
                case 't':  value += '\t'; break;
                case 'r':  value += '\r'; break;
                case '\\': value += '\\'; break;
                case '"':  value += '"';  break;
                case '0':  value += '\0'; break;
                default:
                    diag_.warning({filename_, line_, col_},
                        std::string("unknown escape sequence '\\") + esc + "'");
                    value += esc;
                    break;
            }
        } else {
            value += advance();
        }
    }

    if (!isAtEnd()) {
        advance(); // consume closing "
    }

    return Token{TokenKind::StringLit, value, {filename_, startLine, startCol}};
}

Token Lexer::lexRune() {
    // Opening ' already consumed
    uint32_t startLine = line_;
    uint32_t startCol = col_ - 1;
    std::string value;

    if (!isAtEnd() && peek() == '\\') {
        advance();
        if (!isAtEnd()) {
            char esc = advance();
            switch (esc) {
                case 'n':  value += '\n'; break;
                case 't':  value += '\t'; break;
                case 'r':  value += '\r'; break;
                case '\\': value += '\\'; break;
                case '\'': value += '\''; break;
                case '0':  value += '\0'; break;
                default:   value += esc; break;
            }
        }
    } else if (!isAtEnd()) {
        value += advance();
    }

    if (!isAtEnd() && peek() == '\'') {
        advance();
    } else {
        diag_.error({filename_, startLine, startCol}, "unterminated rune literal");
    }

    return Token{TokenKind::RuneLit, value, {filename_, startLine, startCol}};
}

Token Lexer::lexNumber() {
    uint32_t startLine = line_;
    uint32_t startCol = col_;
    size_t start = pos_;

    // Handle 0x, 0o, 0b prefixes
    if (peek() == '0' && pos_ + 1 < source_.size()) {
        char next = source_[pos_ + 1];
        if (next == 'x' || next == 'X') {
            advance(); advance();
            while (!isAtEnd() && (std::isxdigit(peek()) || peek() == '_')) advance();
            return Token{TokenKind::IntLit, source_.substr(start, pos_ - start), {filename_, startLine, startCol}};
        }
        if (next == 'o' || next == 'O') {
            advance(); advance();
            while (!isAtEnd() && ((peek() >= '0' && peek() <= '7') || peek() == '_')) advance();
            return Token{TokenKind::IntLit, source_.substr(start, pos_ - start), {filename_, startLine, startCol}};
        }
        if (next == 'b' || next == 'B') {
            advance(); advance();
            while (!isAtEnd() && (peek() == '0' || peek() == '1' || peek() == '_')) advance();
            return Token{TokenKind::IntLit, source_.substr(start, pos_ - start), {filename_, startLine, startCol}};
        }
    }

    while (!isAtEnd() && (std::isdigit(peek()) || peek() == '_')) advance();

    bool isFloat = false;
    if (!isAtEnd() && peek() == '.' && peekNext() != '.') {
        isFloat = true;
        advance(); // consume .
        while (!isAtEnd() && (std::isdigit(peek()) || peek() == '_')) advance();
    }

    // Exponent
    if (!isAtEnd() && (peek() == 'e' || peek() == 'E')) {
        isFloat = true;
        advance();
        if (!isAtEnd() && (peek() == '+' || peek() == '-')) advance();
        while (!isAtEnd() && std::isdigit(peek())) advance();
    }

    std::string text = source_.substr(start, pos_ - start);
    TokenKind kind = isFloat ? TokenKind::FloatLit : TokenKind::IntLit;
    return Token{kind, text, {filename_, startLine, startCol}};
}

Token Lexer::lexIdentOrKeyword() {
    uint32_t startLine = line_;
    uint32_t startCol = col_;
    size_t start = pos_;

    while (!isAtEnd() && (std::isalnum(peek()) || peek() == '_')) advance();

    std::string text = source_.substr(start, pos_ - start);

    auto& keywords = keywordMap();
    auto it = keywords.find(text);
    if (it != keywords.end()) {
        return Token{it->second, text, {filename_, startLine, startCol}};
    }

    return Token{TokenKind::Ident, text, {filename_, startLine, startCol}};
}

bool Lexer::shouldInsertSemicolon(TokenKind lastKind) const {
    // Go-style automatic semicolon insertion:
    // Insert semicolon after a line's final token if it is:
    // - an identifier, literal, or one of: break continue fallthrough return
    //   ++ -- ) } ]
    switch (lastKind) {
        case TokenKind::Ident:
        case TokenKind::IntLit:
        case TokenKind::FloatLit:
        case TokenKind::StringLit:
        case TokenKind::RuneLit:
        case TokenKind::True:
        case TokenKind::False:
        case TokenKind::Nil:
        case TokenKind::This:
        case TokenKind::Return:
        case TokenKind::Break:
        case TokenKind::Continue:
        case TokenKind::Fallthrough:
        case TokenKind::PlusPlus:
        case TokenKind::MinusMinus:
        case TokenKind::RParen:
        case TokenKind::RBrace:
        case TokenKind::RBracket:
        case TokenKind::Bang:
        case TokenKind::Question:
            return true;
        default:
            return false;
    }
}

std::vector<Token> Lexer::tokenize() {
    std::vector<Token> tokens;
    TokenKind lastKind = TokenKind::Newline;

    while (!isAtEnd()) {
        skipWhitespace();
        if (isAtEnd()) break;

        char c = peek();

        // Newline — potential semicolon insertion
        if (c == '\n') {
            advance();
            if (shouldInsertSemicolon(lastKind)) {
                tokens.push_back(Token{TokenKind::Semicolon, ";", {filename_, line_ - 1, col_}});
                lastKind = TokenKind::Semicolon;
            }
            continue;
        }

        // Comments
        if (c == '/' && peekNext() == '/') {
            advance(); advance();
            skipLineComment();
            continue;
        }
        if (c == '/' && peekNext() == '*') {
            advance(); advance();
            skipBlockComment();
            continue;
        }

        Token tok = nextToken();
        lastKind = tok.kind;
        tokens.push_back(std::move(tok));
    }

    // Insert final semicolon if needed
    if (shouldInsertSemicolon(lastKind)) {
        tokens.push_back(Token{TokenKind::Semicolon, ";", {filename_, line_, col_}});
    }

    tokens.push_back(Token{TokenKind::Eof, "", {filename_, line_, col_}});
    return tokens;
}

Token Lexer::nextToken() {
    uint32_t startLine = line_;
    uint32_t startCol = col_;
    char c = peek();

    // String literal
    if (c == '"') {
        advance();
        return lexString();
    }

    // Rune literal
    if (c == '\'') {
        advance();
        return lexRune();
    }

    // Number
    if (std::isdigit(c)) {
        return lexNumber();
    }

    // Identifier or keyword
    if (std::isalpha(c) || c == '_') {
        return lexIdentOrKeyword();
    }

    // Operators and punctuation
    advance(); // consume the character
    switch (c) {
        case '(': return Token{TokenKind::LParen,    "(", {filename_, startLine, startCol}};
        case ')': return Token{TokenKind::RParen,    ")", {filename_, startLine, startCol}};
        case '{': return Token{TokenKind::LBrace,    "{", {filename_, startLine, startCol}};
        case '}': return Token{TokenKind::RBrace,    "}", {filename_, startLine, startCol}};
        case '[': return Token{TokenKind::LBracket,  "[", {filename_, startLine, startCol}};
        case ']': return Token{TokenKind::RBracket,  "]", {filename_, startLine, startCol}};
        case ',': return Token{TokenKind::Comma,     ",", {filename_, startLine, startCol}};
        case ';': return Token{TokenKind::Semicolon, ";", {filename_, startLine, startCol}};
        case '~': return Token{TokenKind::Tilde,     "~", {filename_, startLine, startCol}};
        case '?': return Token{TokenKind::Question,  "?", {filename_, startLine, startCol}};

        case '.':
            if (peek() == '.' && peekNext() == '.') {
                advance(); advance();
                return Token{TokenKind::Ellipsis, "...", {filename_, startLine, startCol}};
            }
            return Token{TokenKind::Dot, ".", {filename_, startLine, startCol}};

        case ':':
            if (peek() == ':') {
                advance();
                return Token{TokenKind::ColonColon, "::", {filename_, startLine, startCol}};
            }
            if (peek() == '=') {
                advance();
                return Token{TokenKind::ColonAssign, ":=", {filename_, startLine, startCol}};
            }
            return Token{TokenKind::Colon, ":", {filename_, startLine, startCol}};

        case '+':
            if (peek() == '+') { advance(); return Token{TokenKind::PlusPlus, "++", {filename_, startLine, startCol}}; }
            if (peek() == '=') { advance(); return Token{TokenKind::PlusAssign, "+=", {filename_, startLine, startCol}}; }
            return Token{TokenKind::Plus, "+", {filename_, startLine, startCol}};

        case '-':
            if (peek() == '-') { advance(); return Token{TokenKind::MinusMinus, "--", {filename_, startLine, startCol}}; }
            if (peek() == '=') { advance(); return Token{TokenKind::MinusAssign, "-=", {filename_, startLine, startCol}}; }
            return Token{TokenKind::Minus, "-", {filename_, startLine, startCol}};

        case '*':
            if (peek() == '=') { advance(); return Token{TokenKind::StarAssign, "*=", {filename_, startLine, startCol}}; }
            return Token{TokenKind::Star, "*", {filename_, startLine, startCol}};

        case '/':
            if (peek() == '=') { advance(); return Token{TokenKind::SlashAssign, "/=", {filename_, startLine, startCol}}; }
            return Token{TokenKind::Slash, "/", {filename_, startLine, startCol}};

        case '%':
            if (peek() == '=') { advance(); return Token{TokenKind::PercentAssign, "%=", {filename_, startLine, startCol}}; }
            return Token{TokenKind::Percent, "%", {filename_, startLine, startCol}};

        case '=':
            if (peek() == '=') { advance(); return Token{TokenKind::Equal, "==", {filename_, startLine, startCol}}; }
            return Token{TokenKind::Assign, "=", {filename_, startLine, startCol}};

        case '!':
            if (peek() == '=') { advance(); return Token{TokenKind::NotEqual, "!=", {filename_, startLine, startCol}}; }
            return Token{TokenKind::Bang, "!", {filename_, startLine, startCol}};

        case '<':
            if (peek() == '<') { advance(); return Token{TokenKind::ShiftLeft, "<<", {filename_, startLine, startCol}}; }
            if (peek() == '=') { advance(); return Token{TokenKind::LessEqual, "<=", {filename_, startLine, startCol}}; }
            if (peek() == '-') { advance(); return Token{TokenKind::Arrow, "<-", {filename_, startLine, startCol}}; }
            return Token{TokenKind::Less, "<", {filename_, startLine, startCol}};

        case '>':
            if (peek() == '>') { advance(); return Token{TokenKind::ShiftRight, ">>", {filename_, startLine, startCol}}; }
            if (peek() == '=') { advance(); return Token{TokenKind::GreaterEqual, ">=", {filename_, startLine, startCol}}; }
            return Token{TokenKind::Greater, ">", {filename_, startLine, startCol}};

        case '&':
            if (peek() == '&') { advance(); return Token{TokenKind::AmpAmp, "&&", {filename_, startLine, startCol}}; }
            return Token{TokenKind::Ampersand, "&", {filename_, startLine, startCol}};

        case '|':
            if (peek() == '|') { advance(); return Token{TokenKind::PipePipe, "||", {filename_, startLine, startCol}}; }
            return Token{TokenKind::Pipe, "|", {filename_, startLine, startCol}};

        case '^':
            return Token{TokenKind::Caret, "^", {filename_, startLine, startCol}};

        default:
            diag_.error({filename_, startLine, startCol},
                std::string("unexpected character '") + c + "'");
            return Token{TokenKind::Eof, "", {filename_, startLine, startCol}};
    }
}

} // namespace zo
