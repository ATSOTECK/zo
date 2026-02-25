#include "lexer/token.h"

namespace zo {

const char* tokenKindName(TokenKind kind) {
    switch (kind) {
        case TokenKind::Ident:          return "identifier";
        case TokenKind::IntLit:         return "integer";
        case TokenKind::FloatLit:       return "float";
        case TokenKind::StringLit:      return "string";
        case TokenKind::RuneLit:        return "rune";
        case TokenKind::Package:        return "package";
        case TokenKind::Import:         return "import";
        case TokenKind::Func:           return "func";
        case TokenKind::Return:         return "return";
        case TokenKind::Var:            return "var";
        case TokenKind::Const:          return "const";
        case TokenKind::Type:           return "type";
        case TokenKind::Struct:         return "struct";
        case TokenKind::Impl:           return "impl";
        case TokenKind::Implements:     return "implements";
        case TokenKind::Interface:      return "interface";
        case TokenKind::Enum:           return "enum";
        case TokenKind::Union:          return "union";
        case TokenKind::Match:          return "match";
        case TokenKind::Try:            return "try";
        case TokenKind::If:             return "if";
        case TokenKind::Else:           return "else";
        case TokenKind::For:            return "for";
        case TokenKind::Range:          return "range";
        case TokenKind::Select:         return "select";
        case TokenKind::Go:             return "go";
        case TokenKind::Defer:          return "defer";
        case TokenKind::As:             return "as";
        case TokenKind::This:           return "this";
        case TokenKind::Constraint:     return "constraint";
        case TokenKind::Break:          return "break";
        case TokenKind::Continue:       return "continue";
        case TokenKind::Fallthrough:    return "fallthrough";
        case TokenKind::Error:          return "error";
        case TokenKind::Nil:            return "nil";
        case TokenKind::True:           return "true";
        case TokenKind::False:          return "false";
        case TokenKind::Map:            return "map";
        case TokenKind::Chan:           return "chan";
        case TokenKind::Make:           return "make";
        case TokenKind::Append:         return "append";
        case TokenKind::Len:            return "len";
        case TokenKind::Cap:            return "cap";
        case TokenKind::Plus:           return "+";
        case TokenKind::Minus:          return "-";
        case TokenKind::Star:           return "*";
        case TokenKind::Slash:          return "/";
        case TokenKind::Percent:        return "%";
        case TokenKind::Ampersand:      return "&";
        case TokenKind::Pipe:           return "|";
        case TokenKind::Caret:          return "^";
        case TokenKind::Tilde:          return "~";
        case TokenKind::ShiftLeft:      return "<<";
        case TokenKind::ShiftRight:     return ">>";
        case TokenKind::AmpAmp:        return "&&";
        case TokenKind::PipePipe:       return "||";
        case TokenKind::Bang:           return "!";
        case TokenKind::Assign:         return "=";
        case TokenKind::ColonAssign:    return ":=";
        case TokenKind::ColonColon:     return "::";
        case TokenKind::PlusAssign:     return "+=";
        case TokenKind::MinusAssign:    return "-=";
        case TokenKind::StarAssign:     return "*=";
        case TokenKind::SlashAssign:    return "/=";
        case TokenKind::PercentAssign:  return "%=";
        case TokenKind::Equal:          return "==";
        case TokenKind::NotEqual:       return "!=";
        case TokenKind::Less:           return "<";
        case TokenKind::Greater:        return ">";
        case TokenKind::LessEqual:      return "<=";
        case TokenKind::GreaterEqual:   return ">=";
        case TokenKind::Arrow:          return "<-";
        case TokenKind::PlusPlus:       return "++";
        case TokenKind::MinusMinus:     return "--";
        case TokenKind::Ellipsis:       return "...";
        case TokenKind::FatArrow:       return "=>";
        case TokenKind::LParen:         return "(";
        case TokenKind::RParen:         return ")";
        case TokenKind::LBrace:         return "{";
        case TokenKind::RBrace:         return "}";
        case TokenKind::LBracket:       return "[";
        case TokenKind::RBracket:       return "]";
        case TokenKind::Dot:            return ".";
        case TokenKind::Comma:          return ",";
        case TokenKind::Colon:          return ":";
        case TokenKind::Semicolon:      return ";";
        case TokenKind::Question:       return "?";
        case TokenKind::Newline:        return "newline";
        case TokenKind::Eof:            return "EOF";
    }
    return "unknown";
}

const std::unordered_map<std::string, TokenKind>& keywordMap() {
    static const std::unordered_map<std::string, TokenKind> keywords = {
        {"package",     TokenKind::Package},
        {"import",      TokenKind::Import},
        {"func",        TokenKind::Func},
        {"return",      TokenKind::Return},
        {"var",         TokenKind::Var},
        {"const",       TokenKind::Const},
        {"type",        TokenKind::Type},
        {"struct",      TokenKind::Struct},
        {"impl",        TokenKind::Impl},
        {"implements",  TokenKind::Implements},
        {"interface",   TokenKind::Interface},
        {"enum",        TokenKind::Enum},
        {"union",       TokenKind::Union},
        {"match",       TokenKind::Match},
        {"try",         TokenKind::Try},
        {"if",          TokenKind::If},
        {"else",        TokenKind::Else},
        {"for",         TokenKind::For},
        {"range",       TokenKind::Range},
        {"select",      TokenKind::Select},
        {"go",          TokenKind::Go},
        {"defer",       TokenKind::Defer},
        {"as",          TokenKind::As},
        {"this",        TokenKind::This},
        {"constraint",  TokenKind::Constraint},
        {"break",       TokenKind::Break},
        {"continue",    TokenKind::Continue},
        {"fallthrough", TokenKind::Fallthrough},
        {"error",       TokenKind::Error},
        {"nil",         TokenKind::Nil},
        {"true",        TokenKind::True},
        {"false",       TokenKind::False},
        {"map",         TokenKind::Map},
        {"chan",         TokenKind::Chan},
        {"make",        TokenKind::Make},
        {"append",      TokenKind::Append},
        {"len",         TokenKind::Len},
        {"cap",         TokenKind::Cap},
    };
    return keywords;
}

} // namespace zo
