#pragma once

#include "common/source_location.h"

#include <string>
#include <unordered_map>

namespace zo {

enum class TokenKind {
    // Literals
    Ident,
    IntLit,
    FloatLit,
    StringLit,
    RuneLit,

    // Keywords
    Package,
    Import,
    Func,
    Return,
    Var,
    Const,
    Type,
    Struct,
    Impl,
    Implements,
    Interface,
    Enum,
    Match,
    Try,
    If,
    Else,
    For,
    Range,
    Switch,
    Case,
    Default,
    Select,
    Go,
    Defer,
    As,
    This,
    Constraint,
    Break,
    Continue,
    Fallthrough,
    Nil,
    True,
    False,
    Map,
    Chan,
    Make,
    Append,
    Len,
    Cap,

    // Operators
    Plus,           // +
    Minus,          // -
    Star,           // *
    Slash,          // /
    Percent,        // %
    Ampersand,      // &
    Pipe,           // |
    Caret,          // ^
    Tilde,          // ~
    ShiftLeft,      // <<
    ShiftRight,     // >>
    AmpAmp,        // &&
    PipePipe,       // ||
    Bang,           // !
    Assign,         // =
    ColonAssign,    // :=
    ColonColon,     // ::
    PlusAssign,     // +=
    MinusAssign,    // -=
    StarAssign,     // *=
    SlashAssign,    // /=
    PercentAssign,  // %=
    Equal,          // ==
    NotEqual,       // !=
    Less,           // <
    Greater,        // >
    LessEqual,      // <=
    GreaterEqual,   // >=
    Arrow,          // <-
    PlusPlus,       // ++
    MinusMinus,     // --
    Ellipsis,       // ...

    // Delimiters
    LParen,         // (
    RParen,         // )
    LBrace,         // {
    RBrace,         // }
    LBracket,       // [
    RBracket,       // ]
    Dot,            // .
    Comma,          // ,
    Colon,          // :
    Semicolon,      // ;
    Question,       // ?

    // Special
    Newline,
    Eof,
};

struct Token {
    TokenKind kind;
    std::string text;
    SourceLocation loc;

    bool is(TokenKind k) const { return kind == k; }
    bool isNot(TokenKind k) const { return kind != k; }
};

const char* tokenKindName(TokenKind kind);
const std::unordered_map<std::string, TokenKind>& keywordMap();

} // namespace zo
