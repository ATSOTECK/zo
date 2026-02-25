#include "parser/parser.h"

namespace zo {

Parser::Parser(std::vector<Token> tokens, DiagnosticEngine& diag)
    : tokens_(std::move(tokens)), diag_(diag) {}

// ─── Token stream ───────────────────────────────────────────────────

const Token& Parser::peek() const {
    if (pendingGreater_) {
        static Token syntheticGreater{TokenKind::Greater, ">", {}};
        syntheticGreater.loc = tokens_[pos_].loc;
        return syntheticGreater;
    }
    return tokens_[pos_];
}

const Token& Parser::peekNext() const {
    if (pendingGreater_) return tokens_[pos_];
    if (pos_ + 1 < tokens_.size()) return tokens_[pos_ + 1];
    return tokens_.back();
}

const Token& Parser::advance() {
    if (pendingGreater_) {
        pendingGreater_ = false;
        static Token syntheticGreater{TokenKind::Greater, ">", {}};
        syntheticGreater.loc = tokens_[pos_].loc;
        return syntheticGreater;
    }
    const Token& tok = tokens_[pos_];
    if (pos_ < tokens_.size() - 1) ++pos_;
    return tok;
}

bool Parser::check(TokenKind kind) const {
    if (pendingGreater_) return kind == TokenKind::Greater;
    return peek().kind == kind;
}

bool Parser::match(TokenKind kind) {
    if (check(kind)) {
        advance();
        return true;
    }
    return false;
}

const Token& Parser::expect(TokenKind kind, const std::string& msg) {
    if (check(kind)) return advance();
    diag_.error(peek().loc, msg + ", got '" + peek().text + "'");
    return peek();
}

void Parser::expectSemicolon() {
    if (!match(TokenKind::Semicolon)) {
        if (!check(TokenKind::RBrace) && !check(TokenKind::Eof)) {
            diag_.error(peek().loc, "expected semicolon or newline, got '" + peek().text + "'");
        }
    }
}

void Parser::synchronize() {
    while (!check(TokenKind::Eof)) {
        if (match(TokenKind::Semicolon)) return;
        switch (peek().kind) {
            case TokenKind::Package:
            case TokenKind::Import:
            case TokenKind::Func:
            case TokenKind::Type:
            case TokenKind::Struct:
            case TokenKind::Impl:
            case TokenKind::Enum:
            case TokenKind::Union:
            case TokenKind::Match:
            case TokenKind::Interface:
                return;
            default:
                advance();
        }
    }
}

// ─── File parsing ───────────────────────────────────────────────────

File Parser::parseFile() {
    File file;
    file.filename = peek().loc.file;

    while (!check(TokenKind::Eof)) {
        if (match(TokenKind::Semicolon)) continue;

        try {
            file.decls.push_back(parseDecl());
        } catch (...) {
            synchronize();
        }
    }

    return file;
}

Decl Parser::parseDecl() {
    auto location = loc();

    if (check(TokenKind::Package)) return Decl{parsePackage(), location};
    if (check(TokenKind::Import))  return Decl{parseImport(), location};
    if (check(TokenKind::Func))    return Decl{parseFunc(), location};
    if (check(TokenKind::Impl))    return Decl{parseImpl(), location};
    if (check(TokenKind::Enum))    return Decl{parseEnum(), location};
    if (check(TokenKind::Union))   return Decl{parseUnion(), location};
    if (check(TokenKind::Type))    return parseTypeDecl();

    diag_.error(peek().loc, "expected declaration, got '" + peek().text + "'");
    advance();
    return Decl{decl::Package{""}, location};
}

// ─── Top-level declarations ─────────────────────────────────────────

decl::Package Parser::parsePackage() {
    expect(TokenKind::Package, "expected 'package'");
    auto name = expect(TokenKind::Ident, "expected package name").text;
    expectSemicolon();
    return decl::Package{name};
}

decl::Import Parser::parseImport() {
    expect(TokenKind::Import, "expected 'import'");
    auto name = expect(TokenKind::Ident, "expected import name").text;

    std::optional<std::string> alias;
    if (match(TokenKind::As)) {
        alias = expect(TokenKind::Ident, "expected alias name").text;
    }

    expectSemicolon();
    return decl::Import{name, alias};
}

decl::Func Parser::parseFunc() {
    expect(TokenKind::Func, "expected 'func'");
    auto name = expect(TokenKind::Ident, "expected function name").text;

    std::vector<TypeParam> typeParams;
    if (check(TokenKind::Less)) {
        typeParams = parseTypeParams();
    }

    auto params = parseParams();
    auto returns = parseReturnTypes();

    std::vector<StmtPtr> body;
    if (check(TokenKind::LBrace)) {
        auto block = parseBlock();
        body = std::move(block->stmts);
    }
    expectSemicolon();

    return decl::Func{name, std::move(typeParams), std::move(params), std::move(returns), std::move(body)};
}

Decl Parser::parseTypeDecl() {
    auto location = loc();
    expect(TokenKind::Type, "expected 'type'");
    auto name = expect(TokenKind::Ident, "expected type name").text;

    // type X constraint { int | f32 | f64 }
    if (check(TokenKind::Constraint)) {
        advance();
        expect(TokenKind::LBrace, "expected '{'");
        std::vector<TypeRefPtr> types;
        types.push_back(parseType());
        while (match(TokenKind::Pipe)) {
            types.push_back(parseType());
        }
        expect(TokenKind::RBrace, "expected '}'");
        expectSemicolon();
        return Decl{decl::Constraint{name, std::move(types)}, location};
    }

    // type X error { ... }
    if (check(TokenKind::Error)) {
        advance();
        auto errorEnum = parseErrorEnum(name);
        return Decl{std::move(errorEnum), location};
    }

    // Optional type params: type Stack<T> struct { ... }
    std::vector<TypeParam> typeParams;
    if (check(TokenKind::Less)) {
        typeParams = parseTypeParams();
    }

    // type X struct { ... }
    if (check(TokenKind::Struct)) {
        advance();
        auto s = parseStructBody(name);
        s.type_params = std::move(typeParams);
        return Decl{std::move(s), location};
    }

    // type X interface { ... }
    if (check(TokenKind::Interface)) {
        if (!typeParams.empty()) {
            diag_.error(location, "type parameters not allowed on interfaces");
        }
        advance();
        expect(TokenKind::LBrace, "expected '{'");
        std::vector<decl::Func> methods;
        while (!check(TokenKind::RBrace) && !check(TokenKind::Eof)) {
            if (match(TokenKind::Semicolon)) continue;
            auto methodName = expect(TokenKind::Ident, "expected method name").text;
            auto mParams = parseParams();
            auto mReturns = parseReturnTypes();
            expectSemicolon();
            methods.push_back(decl::Func{methodName, {}, std::move(mParams), std::move(mReturns), {}});
        }
        expect(TokenKind::RBrace, "expected '}'");
        expectSemicolon();
        return Decl{decl::Interface{name, std::move(methods)}, location};
    }

    // type X <other type>
    if (!typeParams.empty()) {
        diag_.error(location, "type parameters not allowed on type aliases");
    }
    auto type = parseType();
    expectSemicolon();
    return Decl{decl::TypeAlias{name, std::move(type)}, location};
}

decl::Struct Parser::parseStructBody(const std::string& name) {
    expect(TokenKind::LBrace, "expected '{'");

    std::vector<Field> fields;
    while (!check(TokenKind::RBrace) && !check(TokenKind::Eof)) {
        if (match(TokenKind::Semicolon)) continue;
        auto fieldLoc = loc();
        auto fieldName = expect(TokenKind::Ident, "expected field name").text;
        auto fieldType = parseType();
        fields.push_back(Field{fieldName, std::move(fieldType), fieldLoc});
        expectSemicolon();
    }
    expect(TokenKind::RBrace, "expected '}'");
    expectSemicolon();

    return decl::Struct{name, {}, std::move(fields)};
}

decl::Interface Parser::parseInterface() {
    expect(TokenKind::Interface, "expected 'interface'");
    auto name = expect(TokenKind::Ident, "expected interface name").text;
    expect(TokenKind::LBrace, "expected '{'");

    std::vector<decl::Func> methods;
    while (!check(TokenKind::RBrace) && !check(TokenKind::Eof)) {
        if (match(TokenKind::Semicolon)) continue;
        auto methodName = expect(TokenKind::Ident, "expected method name").text;
        auto params = parseParams();
        auto returns = parseReturnTypes();
        expectSemicolon();
        methods.push_back(decl::Func{methodName, {}, std::move(params), std::move(returns), {}});
    }
    expect(TokenKind::RBrace, "expected '}'");
    expectSemicolon();

    return decl::Interface{name, std::move(methods)};
}

decl::ImplBlock Parser::parseImpl() {
    expect(TokenKind::Impl, "expected 'impl'");

    auto target = expect(TokenKind::Ident, "expected type name").text;

    // Parse target type args: impl Stack<T> { ... }
    std::vector<TypeParam> typeParams;
    std::vector<std::string> targetTypeArgs;
    if (check(TokenKind::Less)) {
        expect(TokenKind::Less, "expected '<'");
        targetTypeArgs.push_back(expect(TokenKind::Ident, "expected type argument").text);
        while (match(TokenKind::Comma)) {
            targetTypeArgs.push_back(expect(TokenKind::Ident, "expected type argument").text);
        }
        if (check(TokenKind::Greater)) {
            advance();
        } else if (check(TokenKind::ShiftRight)) {
            advance();
            pendingGreater_ = true;
        } else {
            expect(TokenKind::Greater, "expected '>'");
        }
        // Infer type params from target type args
        for (const auto& arg : targetTypeArgs) {
            typeParams.push_back({arg, std::nullopt, {}});
        }
    }

    std::optional<std::string> iface;
    if (match(TokenKind::Implements)) {
        iface = expect(TokenKind::Ident, "expected interface name").text;
    }

    expect(TokenKind::LBrace, "expected '{'");

    std::vector<decl::Func> methods;
    while (!check(TokenKind::RBrace) && !check(TokenKind::Eof)) {
        if (match(TokenKind::Semicolon)) continue;
        methods.push_back(parseFunc());
    }
    expect(TokenKind::RBrace, "expected '}'");
    expectSemicolon();

    return decl::ImplBlock{target, std::move(typeParams), std::move(targetTypeArgs), iface, std::move(methods)};
}

// ─── Enum & Union declarations ──────────────────────────────────────

decl::Enum Parser::parseEnum() {
    expect(TokenKind::Enum, "expected 'enum'");
    auto name = expect(TokenKind::Ident, "expected enum name").text;

    // Optional underlying type: enum Color: f32 { ... }
    std::optional<TypeRefPtr> underlying;
    if (match(TokenKind::Colon)) {
        underlying = parseType();
    }

    expect(TokenKind::LBrace, "expected '{'");

    std::vector<EnumVariantDef> variants;
    while (!check(TokenKind::RBrace) && !check(TokenKind::Eof)) {
        if (match(TokenKind::Semicolon)) continue;

        auto varLoc = loc();
        auto varName = expect(TokenKind::Ident, "expected variant name").text;

        std::optional<ExprPtr> value;
        if (match(TokenKind::Assign)) {
            value = parseExpr();
        }

        variants.push_back(EnumVariantDef{varName, std::move(value), varLoc});

        // Allow comma, semicolon, or newline between variants
        if (!check(TokenKind::RBrace)) {
            if (!match(TokenKind::Comma)) {
                match(TokenKind::Semicolon);
            }
        }
    }

    expect(TokenKind::RBrace, "expected '}'");
    expectSemicolon();

    return decl::Enum{name, std::move(underlying), std::move(variants)};
}

decl::Union Parser::parseUnion() {
    expect(TokenKind::Union, "expected 'union'");
    auto name = expect(TokenKind::Ident, "expected union name").text;

    expect(TokenKind::LBrace, "expected '{'");

    std::vector<UnionVariantDef> variants;
    while (!check(TokenKind::RBrace) && !check(TokenKind::Eof)) {
        if (match(TokenKind::Semicolon)) continue;

        auto varLoc = loc();
        auto varName = expect(TokenKind::Ident, "expected variant name").text;

        std::vector<Field> fields;
        if (match(TokenKind::LParen)) {
            // Parse fields: name type, name type, ...
            if (!check(TokenKind::RParen)) {
                auto fLoc = loc();
                auto fName = expect(TokenKind::Ident, "expected field name").text;
                auto fType = parseType();
                fields.push_back(Field{fName, std::move(fType), fLoc});
                while (match(TokenKind::Comma)) {
                    fLoc = loc();
                    fName = expect(TokenKind::Ident, "expected field name").text;
                    fType = parseType();
                    fields.push_back(Field{fName, std::move(fType), fLoc});
                }
            }
            expect(TokenKind::RParen, "expected ')'");
        }

        variants.push_back(UnionVariantDef{varName, std::move(fields), varLoc});

        // Allow comma, semicolon, or newline between variants
        if (!check(TokenKind::RBrace)) {
            if (!match(TokenKind::Comma)) {
                match(TokenKind::Semicolon);
            }
        }
    }

    expect(TokenKind::RBrace, "expected '}'");
    expectSemicolon();

    return decl::Union{name, std::move(variants)};
}

decl::ErrorEnum Parser::parseErrorEnum(const std::string& name) {
    expect(TokenKind::LBrace, "expected '{'");

    std::vector<EnumVariantDef> variants;
    while (!check(TokenKind::RBrace) && !check(TokenKind::Eof)) {
        if (match(TokenKind::Semicolon)) continue;

        auto varLoc = loc();
        auto varName = expect(TokenKind::Ident, "expected variant name").text;

        variants.push_back(EnumVariantDef{varName, std::nullopt, varLoc});

        if (!check(TokenKind::RBrace)) {
            if (!match(TokenKind::Comma)) {
                match(TokenKind::Semicolon);
            }
        }
    }

    expect(TokenKind::RBrace, "expected '}'");
    expectSemicolon();

    return decl::ErrorEnum{name, std::move(variants)};
}

// ─── Match statement ────────────────────────────────────────────────

StmtPtr Parser::parseMatch() {
    auto location = loc();
    expect(TokenKind::Match, "expected 'match'");

    // Parse the tag expression (disable composite literals to avoid { ambiguity)
    auto savedCL = compositeLitOk_;
    compositeLitOk_ = false;
    auto value = parseExpr();
    compositeLitOk_ = savedCL;

    expect(TokenKind::LBrace, "expected '{'");

    std::vector<MatchArm> arms;
    while (!check(TokenKind::RBrace) && !check(TokenKind::Eof)) {
        if (match(TokenKind::Semicolon)) continue;

        auto armLoc = loc();
        std::vector<ExprPtr> patterns;

        // Parse pattern(s) separated by commas
        // Pattern: .Variant, Type.Variant, _, .Variant(bindings), or literal
        auto parsePattern = [this]() -> ExprPtr {
            auto patLoc = loc();

            // _ wildcard
            if (check(TokenKind::Ident) && peek().text == "_") {
                advance();
                return makeExpr<expr::Ident>(patLoc, std::string("_"));
            }

            // .Variant or .Variant(bindings)
            if (check(TokenKind::Dot)) {
                advance();
                auto variant = expect(TokenKind::Ident, "expected variant name after '.'").text;

                // Check for destructuring: .Variant(a, b)
                if (check(TokenKind::LParen)) {
                    advance();
                    std::vector<std::string> bindings;
                    if (!check(TokenKind::RParen)) {
                        bindings.push_back(expect(TokenKind::Ident, "expected binding name").text);
                        while (match(TokenKind::Comma)) {
                            bindings.push_back(expect(TokenKind::Ident, "expected binding name").text);
                        }
                    }
                    expect(TokenKind::RParen, "expected ')'");
                    return makeExpr<expr::UnionVariant>(patLoc,
                        std::optional<std::string>{}, variant, std::move(bindings));
                }

                return makeExpr<expr::EnumVariant>(patLoc,
                    std::optional<std::string>{}, variant);
            }

            // Type.Variant
            if (check(TokenKind::Ident) && peekNext().kind == TokenKind::Dot) {
                auto typeName = advance().text;
                advance(); // consume .
                auto variant = expect(TokenKind::Ident, "expected variant name").text;

                // Check for destructuring
                if (check(TokenKind::LParen)) {
                    advance();
                    std::vector<std::string> bindings;
                    if (!check(TokenKind::RParen)) {
                        bindings.push_back(expect(TokenKind::Ident, "expected binding name").text);
                        while (match(TokenKind::Comma)) {
                            bindings.push_back(expect(TokenKind::Ident, "expected binding name").text);
                        }
                    }
                    expect(TokenKind::RParen, "expected ')'");
                    return makeExpr<expr::UnionVariant>(patLoc,
                        std::optional<std::string>{typeName}, variant, std::move(bindings));
                }

                return makeExpr<expr::EnumVariant>(patLoc,
                    std::optional<std::string>{typeName}, variant);
            }

            // Literal expression (int, string, etc.)
            return parseExpr();
        };

        patterns.push_back(parsePattern());
        while (match(TokenKind::Comma)) {
            // Check if next token starts a pattern (not the body)
            if (check(TokenKind::RBrace) || check(TokenKind::Eof)) break;
            patterns.push_back(parsePattern());
        }

        expect(TokenKind::Colon, "expected ':' after match pattern");

        // Parse body statements until next pattern or }
        std::vector<StmtPtr> body;
        while (!check(TokenKind::RBrace) && !check(TokenKind::Eof)) {
            // Check if this looks like a new arm
            // .Variant pattern
            if (check(TokenKind::Dot) && peekNext().kind == TokenKind::Ident) break;
            // _ wildcard pattern
            if (check(TokenKind::Ident) && peek().text == "_" &&
                (peekNext().kind == TokenKind::Colon || peekNext().kind == TokenKind::Comma)) break;
            // Type.Variant pattern
            if (check(TokenKind::Ident) && peekNext().kind == TokenKind::Dot) break;
            // Literal pattern: scan ahead for : at depth 0
            if (check(TokenKind::IntLit) || check(TokenKind::FloatLit) ||
                check(TokenKind::StringLit) || check(TokenKind::True) ||
                check(TokenKind::False) || check(TokenKind::Nil)) {
                // Lookahead: check if we see pattern , pattern : or pattern :
                size_t savedScan = pos_;
                bool looksLikeArm = false;
                int depth = 0;
                while (savedScan < tokens_.size()) {
                    auto k = tokens_[savedScan].kind;
                    if (k == TokenKind::LParen || k == TokenKind::LBracket) { depth++; savedScan++; continue; }
                    if (k == TokenKind::RParen || k == TokenKind::RBracket) { depth--; savedScan++; continue; }
                    if (depth == 0 && k == TokenKind::Colon) { looksLikeArm = true; break; }
                    if (k == TokenKind::Semicolon || k == TokenKind::LBrace || k == TokenKind::RBrace) break;
                    savedScan++;
                }
                if (looksLikeArm) break;
            }

            if (match(TokenKind::Semicolon)) continue;
            body.push_back(parseStmt());
        }

        arms.push_back(MatchArm{std::move(patterns), std::move(body), armLoc});
    }

    expect(TokenKind::RBrace, "expected '}'");
    expectSemicolon();

    return makeStmt<stmt::Match>(location, std::move(value), std::move(arms));
}

// ─── Generics ───────────────────────────────────────────────────────

std::vector<TypeParam> Parser::parseTypeParams() {
    // < Ident [: Constraint] [, Ident [: Constraint]]* >
    expect(TokenKind::Less, "expected '<'");
    std::vector<TypeParam> params;

    auto paramLoc = loc();
    auto name = expect(TokenKind::Ident, "expected type parameter name").text;
    std::optional<std::string> constraint;
    if (match(TokenKind::Colon)) {
        constraint = expect(TokenKind::Ident, "expected constraint name").text;
    }
    params.push_back(TypeParam{name, constraint, paramLoc});

    while (match(TokenKind::Comma)) {
        paramLoc = loc();
        name = expect(TokenKind::Ident, "expected type parameter name").text;
        constraint = std::nullopt;
        if (match(TokenKind::Colon)) {
            constraint = expect(TokenKind::Ident, "expected constraint name").text;
        }
        params.push_back(TypeParam{name, constraint, paramLoc});
    }

    // Accept > or >> (split >> into pending >)
    if (check(TokenKind::Greater)) {
        advance();
    } else if (check(TokenKind::ShiftRight)) {
        advance();
        pendingGreater_ = true;
    } else {
        expect(TokenKind::Greater, "expected '>'");
    }

    return params;
}

std::vector<TypeRefPtr> Parser::parseTypeArgs() {
    // < Type [, Type]* >
    expect(TokenKind::Less, "expected '<'");
    std::vector<TypeRefPtr> args;

    args.push_back(parseType());
    while (match(TokenKind::Comma)) {
        args.push_back(parseType());
    }

    // Accept > or >> (split >> into pending >)
    if (check(TokenKind::Greater)) {
        advance();
    } else if (check(TokenKind::ShiftRight)) {
        advance();
        pendingGreater_ = true;
    } else {
        expect(TokenKind::Greater, "expected '>'");
    }

    return args;
}

bool Parser::looksLikeTypeArgs() const {
    // Scan forward from current '<' to find matching '>' and check
    // if it's followed by '(' or '{' (generic call or composite lit).
    // Handles nested <> and basic type syntax (idents, ::, commas, *, []).
    if (!check(TokenKind::Less)) return false;
    size_t i = pos_ + 1;  // skip '<'
    int depth = 1;
    while (i < tokens_.size() && depth > 0) {
        auto k = tokens_[i].kind;
        if (k == TokenKind::Less) {
            depth++;
        } else if (k == TokenKind::Greater) {
            depth--;
            if (depth == 0) break;
        } else if (k == TokenKind::ShiftRight) {
            depth -= 2;
            if (depth <= 0) break;
        } else if (k == TokenKind::Ident || k == TokenKind::Comma ||
                   k == TokenKind::Star || k == TokenKind::LBracket ||
                   k == TokenKind::RBracket || k == TokenKind::ColonColon ||
                   k == TokenKind::Map || k == TokenKind::Chan ||
                   k == TokenKind::Func || k == TokenKind::Colon ||
                   k == TokenKind::Dot) {
            // Valid inside type args
        } else if (k == TokenKind::Semicolon || k == TokenKind::Eof ||
                   k == TokenKind::LBrace || k == TokenKind::RBrace) {
            return false;  // Definitely not type args
        } else {
            return false;  // Unexpected token — not type args
        }
        i++;
    }
    if (depth != 0) return false;
    // Check what follows the closing '>'
    size_t next = i + 1;
    if (next < tokens_.size()) {
        auto k = tokens_[next].kind;
        return k == TokenKind::LParen || k == TokenKind::LBrace;
    }
    return false;
}

// ─── Types ──────────────────────────────────────────────────────────

std::vector<TypeRefPtr> Parser::parseReturnTypes() {
    std::vector<TypeRefPtr> returns;

    // No return type
    if (check(TokenKind::LBrace) || check(TokenKind::Semicolon) || check(TokenKind::Eof)) {
        return returns;
    }

    // Multiple return types: (T1, T2, ...)
    if (check(TokenKind::LParen)) {
        advance();
        if (!check(TokenKind::RParen)) {
            returns.push_back(parseType());
            while (match(TokenKind::Comma)) {
                returns.push_back(parseType());
            }
        }
        expect(TokenKind::RParen, "expected ')'");
        return returns;
    }

    // Single return type
    returns.push_back(parseType());
    return returns;
}

TypeRefPtr Parser::parseType() {
    auto base = parseBaseType();

    // Postfix type modifiers: ! and ?
    while (true) {
        if (check(TokenKind::Bang)) {
            auto bangLoc = loc();
            advance();
            base = std::make_unique<TypeRef>(TypeRef{type_ref::Result{std::move(base)}, bangLoc});
        } else if (check(TokenKind::Question)) {
            auto qLoc = loc();
            advance();
            base = std::make_unique<TypeRef>(TypeRef{type_ref::Optional{std::move(base)}, qLoc});
        } else {
            break;
        }
    }

    return base;
}

TypeRefPtr Parser::parseBaseType() {
    auto location = loc();

    // Pointer: *T
    if (match(TokenKind::Star)) {
        return std::make_unique<TypeRef>(TypeRef{type_ref::Pointer{parseType()}, location});
    }

    // Slice or Array: []T or [N]T
    if (match(TokenKind::LBracket)) {
        if (match(TokenKind::RBracket)) {
            return std::make_unique<TypeRef>(TypeRef{type_ref::Slice{parseType()}, location});
        }
        auto size = parseExpr();
        expect(TokenKind::RBracket, "expected ']'");
        return std::make_unique<TypeRef>(TypeRef{type_ref::Array{std::move(size), parseType()}, location});
    }

    // Map: map[K]V
    if (check(TokenKind::Map)) {
        advance();
        expect(TokenKind::LBracket, "expected '['");
        auto key = parseType();
        expect(TokenKind::RBracket, "expected ']'");
        auto value = parseType();
        return std::make_unique<TypeRef>(TypeRef{type_ref::MapType{std::move(key), std::move(value)}, location});
    }

    // Channel types: chan T, <-chan T, chan<- T
    if (check(TokenKind::Arrow)) {
        // <-chan T (receive-only)
        advance();
        expect(TokenKind::Chan, "expected 'chan' after '<-'");
        auto elem = parseType();
        return std::make_unique<TypeRef>(TypeRef{type_ref::Channel{ChanDir::Recv, std::move(elem)}, location});
    }
    if (check(TokenKind::Chan)) {
        advance();
        if (check(TokenKind::Arrow)) {
            // chan<- T (send-only)
            advance();
            auto elem = parseType();
            return std::make_unique<TypeRef>(TypeRef{type_ref::Channel{ChanDir::Send, std::move(elem)}, location});
        }
        // chan T (bidirectional)
        auto elem = parseType();
        return std::make_unique<TypeRef>(TypeRef{type_ref::Channel{ChanDir::Both, std::move(elem)}, location});
    }

    // Function type: func(params) returns
    if (check(TokenKind::Func)) {
        advance();
        auto params_list = parseParams();
        std::vector<TypeRefPtr> paramTypes;
        for (auto& p : params_list) {
            paramTypes.push_back(std::move(p.type));
        }
        auto ret = parseReturnTypes();
        return std::make_unique<TypeRef>(TypeRef{
            type_ref::FuncType{std::move(paramTypes), std::move(ret)}, location});
    }

    // Named type (possibly qualified with ::, possibly generic with <Args>)
    if (check(TokenKind::Ident)) {
        auto name = advance().text;
        if (match(TokenKind::ColonColon)) {
            auto member = expect(TokenKind::Ident, "expected type name after '::'").text;
            auto base = makeTypeRef<type_ref::Qualified>(location, name, member);
            if (check(TokenKind::Less)) {
                auto args = parseTypeArgs();
                return makeTypeRef<type_ref::Generic>(location, std::move(base), std::move(args));
            }
            return base;
        }
        auto base = makeTypeRef<type_ref::Named>(location, name);
        if (check(TokenKind::Less)) {
            auto args = parseTypeArgs();
            return makeTypeRef<type_ref::Generic>(location, std::move(base), std::move(args));
        }
        return base;
    }

    diag_.error(location, "expected type, got '" + peek().text + "'");
    return makeTypeRef<type_ref::Named>(location, "error");
}

// ─── Statements ─────────────────────────────────────────────────────

std::unique_ptr<stmt::Block> Parser::parseBlock() {
    expect(TokenKind::LBrace, "expected '{'");
    std::vector<StmtPtr> stmts;

    while (!check(TokenKind::RBrace) && !check(TokenKind::Eof)) {
        if (match(TokenKind::Semicolon)) continue;
        stmts.push_back(parseStmt());
    }

    expect(TokenKind::RBrace, "expected '}'");
    return std::make_unique<stmt::Block>(stmt::Block{std::move(stmts)});
}

StmtPtr Parser::parseStmt() {
    auto location = loc();

    if (check(TokenKind::Return))      return parseReturn();
    if (check(TokenKind::Var))         return parseVarDecl();
    if (check(TokenKind::Const))       return parseConstDecl();
    if (check(TokenKind::If))          return parseIf();
    if (check(TokenKind::For))         return parseFor();
    if (check(TokenKind::Select))      return parseSelect();
    if (check(TokenKind::Match))       return parseMatch();
    if (check(TokenKind::Break)) {
        advance();
        expectSemicolon();
        return makeStmt<stmt::Break>(location);
    }
    if (check(TokenKind::Continue)) {
        advance();
        expectSemicolon();
        return makeStmt<stmt::Continue>(location);
    }
    if (check(TokenKind::Fallthrough)) {
        advance();
        expectSemicolon();
        return makeStmt<stmt::Fallthrough>(location);
    }
    if (check(TokenKind::Go)) {
        advance();
        auto call = parseExpr();
        expectSemicolon();
        return makeStmt<stmt::GoStmt>(location, std::move(call));
    }
    if (check(TokenKind::Defer)) {
        advance();
        auto call = parseExpr();
        expectSemicolon();
        return makeStmt<stmt::DeferStmt>(location, std::move(call));
    }
    if (check(TokenKind::LBrace)) {
        auto block = parseBlock();
        expectSemicolon();
        return makeStmt<stmt::Block>(location, std::move(block->stmts));
    }

    return parseSimpleStmt();
}

StmtPtr Parser::parseReturn() {
    auto location = loc();
    expect(TokenKind::Return, "expected 'return'");

    std::vector<ExprPtr> values;
    if (!check(TokenKind::Semicolon) && !check(TokenKind::RBrace) && !check(TokenKind::Eof)) {
        values.push_back(parseExpr());
        while (match(TokenKind::Comma)) {
            values.push_back(parseExpr());
        }
    }

    expectSemicolon();
    return makeStmt<stmt::Return>(location, std::move(values));
}

StmtPtr Parser::parseVarDecl() {
    auto location = loc();
    expect(TokenKind::Var, "expected 'var'");
    auto name = expect(TokenKind::Ident, "expected variable name").text;

    std::optional<TypeRefPtr> type;
    std::optional<ExprPtr> init;

    if (!check(TokenKind::Assign) && !check(TokenKind::Semicolon)) {
        type = parseType();
    }
    if (match(TokenKind::Assign)) {
        init = parseExpr();
    }

    expectSemicolon();
    return makeStmt<stmt::VarDecl>(location, name, std::move(type), std::move(init));
}

StmtPtr Parser::parseConstDecl() {
    auto location = loc();
    expect(TokenKind::Const, "expected 'const'");
    auto name = expect(TokenKind::Ident, "expected constant name").text;

    std::optional<TypeRefPtr> type;
    if (!check(TokenKind::Assign)) {
        type = parseType();
    }

    expect(TokenKind::Assign, "expected '='");
    auto init = parseExpr();
    expectSemicolon();

    return makeStmt<stmt::ConstDecl>(location, name, std::move(type), std::move(init));
}

StmtPtr Parser::parseIf() {
    auto location = loc();
    expect(TokenKind::If, "expected 'if'");

    // Disable composite literals in condition to avoid { ambiguity
    auto savedCL = compositeLitOk_;
    compositeLitOk_ = false;
    auto cond = parseExpr();
    compositeLitOk_ = savedCL;

    auto then = parseBlock();

    std::optional<StmtPtr> elseBlock;
    if (match(TokenKind::Else)) {
        if (check(TokenKind::If)) {
            elseBlock = parseIf();
            // Inner parseIf() already consumed the trailing semicolon
            return makeStmt<stmt::If>(location, std::move(cond), std::move(then), std::move(elseBlock));
        } else {
            auto block = parseBlock();
            elseBlock = makeStmt<stmt::Block>(loc(), std::move(block->stmts));
        }
    }

    expectSemicolon();
    return makeStmt<stmt::If>(location, std::move(cond), std::move(then), std::move(elseBlock));
}

StmtPtr Parser::parseFor() {
    auto location = loc();
    expect(TokenKind::For, "expected 'for'");

    // for { ... } — infinite loop
    if (check(TokenKind::LBrace)) {
        auto body = parseBlock();
        expectSemicolon();
        return makeStmt<stmt::For>(location,
            std::optional<StmtPtr>{}, std::optional<ExprPtr>{},
            std::optional<StmtPtr>{}, std::move(body));
    }

    // for range expr { ... } — range with no variables
    if (check(TokenKind::Range)) {
        advance();
        auto iterable = parseExpr();
        auto body = parseBlock();
        expectSemicolon();
        return makeStmt<stmt::ForRange>(location,
            "_", std::optional<std::string>{}, true, std::move(iterable), std::move(body));
    }

    // Disable composite literals in for clause
    auto savedCL = compositeLitOk_;
    compositeLitOk_ = false;

    // Try to detect for-range: for k, v := range expr { ... }
    // or for k := range expr { ... }
    // Look ahead: if we see Ident [:= | ,] ... range, it's a for-range
    size_t savedPos = pos_;
    bool isRange = false;

    if (check(TokenKind::Ident)) {
        auto firstName = advance().text;
        std::optional<std::string> secondName;

        if (match(TokenKind::Comma)) {
            if (check(TokenKind::Ident)) {
                secondName = advance().text;
            }
        }

        if (match(TokenKind::ColonAssign)) {
            if (check(TokenKind::Range)) {
                advance(); // consume 'range'
                auto iterable = parseExpr();
                compositeLitOk_ = savedCL;
                auto body = parseBlock();
                expectSemicolon();
                return makeStmt<stmt::ForRange>(location,
                    firstName, secondName, true, std::move(iterable), std::move(body));
            }
        }

        // Not a for-range — backtrack
        pos_ = savedPos;
    }

    // Parse first expression
    auto firstExpr = parseExpr();

    // for cond { ... }
    if (check(TokenKind::LBrace)) {
        compositeLitOk_ = savedCL;
        auto body = parseBlock();
        expectSemicolon();
        return makeStmt<stmt::For>(location,
            std::optional<StmtPtr>{}, std::optional<ExprPtr>{std::move(firstExpr)},
            std::optional<StmtPtr>{}, std::move(body));
    }

    // Check for short decl in init: x := expr; cond; post
    StmtPtr init;
    if (check(TokenKind::ColonAssign)) {
        advance();
        std::vector<std::string> names;
        if (auto* ident = std::get_if<expr::Ident>(&firstExpr->kind)) {
            names.push_back(ident->name);
        } else {
            names.push_back("_");
        }
        auto value = parseExpr();
        init = makeStmt<stmt::ExprStmt>(location,
            std::make_unique<Expr>(Expr{expr::ShortDecl{std::move(names), std::move(value)}, location}));
    } else if (check(TokenKind::Assign) || check(TokenKind::PlusAssign) ||
               check(TokenKind::MinusAssign)) {
        auto op = advance().kind;
        auto value = parseExpr();
        init = makeStmt<stmt::ExprStmt>(location,
            std::make_unique<Expr>(Expr{expr::Assign{std::move(firstExpr), op, std::move(value)}, location}));
    } else {
        init = makeStmt<stmt::ExprStmt>(location, std::move(firstExpr));
    }

    expect(TokenKind::Semicolon, "expected ';'");

    std::optional<ExprPtr> cond;
    if (!check(TokenKind::Semicolon)) {
        cond = parseExpr();
    }
    expect(TokenKind::Semicolon, "expected ';'");

    std::optional<StmtPtr> post;
    if (!check(TokenKind::LBrace)) {
        // Parse post statement without consuming semicolon
        auto postLoc = loc();
        auto postExpr = parseExpr();
        if (check(TokenKind::PlusPlus) || check(TokenKind::MinusMinus)) {
            auto op = advance().kind;
            post = makeStmt<stmt::IncDec>(postLoc, std::move(postExpr), op);
        } else if (check(TokenKind::ColonAssign)) {
            advance();
            std::vector<std::string> names;
            if (auto* ident = std::get_if<expr::Ident>(&postExpr->kind)) {
                names.push_back(ident->name);
            }
            auto val = parseExpr();
            post = makeStmt<stmt::ExprStmt>(postLoc,
                std::make_unique<Expr>(Expr{expr::ShortDecl{std::move(names), std::move(val)}, postLoc}));
        } else if (check(TokenKind::Assign) || check(TokenKind::PlusAssign)) {
            auto op = advance().kind;
            auto val = parseExpr();
            post = makeStmt<stmt::ExprStmt>(postLoc,
                std::make_unique<Expr>(Expr{expr::Assign{std::move(postExpr), op, std::move(val)}, postLoc}));
        } else {
            post = makeStmt<stmt::ExprStmt>(postLoc, std::move(postExpr));
        }
    }

    compositeLitOk_ = savedCL;
    auto body = parseBlock();
    expectSemicolon();
    return makeStmt<stmt::For>(location, std::move(init), std::move(cond), std::move(post), std::move(body));
}

StmtPtr Parser::parseSelect() {
    auto location = loc();
    expect(TokenKind::Select, "expected 'select'");
    expect(TokenKind::LBrace, "expected '{'");

    std::vector<SelectCase> cases;
    while (!check(TokenKind::RBrace) && !check(TokenKind::Eof)) {
        if (match(TokenKind::Semicolon)) continue;

        std::optional<StmtPtr> comm;

        // Default arm: _ :
        if (check(TokenKind::Ident) && peek().text == "_" &&
            peekNext().kind == TokenKind::Colon) {
            advance(); // consume _
            expect(TokenKind::Colon, "expected ':'");
            // comm stays nullopt = default
        } else {
            // Communication clause (ends with : not ;)
            auto commLoc = loc();
            auto commExpr = parseExpr();

            // Check for send: expr <- expr
            if (check(TokenKind::Arrow)) {
                advance();
                auto value = parseExpr();
                comm = makeStmt<stmt::Send>(commLoc, std::move(commExpr), std::move(value));
            }
            // Check for short decl: x := <-ch  or  x, ok := <-ch
            else if (check(TokenKind::Comma) || check(TokenKind::ColonAssign)) {
                std::vector<std::string> names;
                if (auto* ident = std::get_if<expr::Ident>(&commExpr->kind)) {
                    names.push_back(ident->name);
                } else {
                    names.push_back("_");
                }
                while (match(TokenKind::Comma)) {
                    auto nextExpr = parseExpr();
                    if (auto* ident = std::get_if<expr::Ident>(&nextExpr->kind)) {
                        names.push_back(ident->name);
                    } else {
                        names.push_back("_");
                    }
                }
                if (match(TokenKind::ColonAssign)) {
                    auto value = parseExpr();
                    comm = makeStmt<stmt::ExprStmt>(commLoc,
                        std::make_unique<Expr>(Expr{expr::ShortDecl{std::move(names), std::move(value)}, commLoc}));
                } else if (match(TokenKind::Assign)) {
                    auto value = parseExpr();
                    comm = makeStmt<stmt::ExprStmt>(commLoc,
                        std::make_unique<Expr>(Expr{expr::Assign{std::move(commExpr), TokenKind::Assign, std::move(value)}, commLoc}));
                }
            }
            // Assignment: x = <-ch
            else if (check(TokenKind::Assign)) {
                advance();
                auto value = parseExpr();
                comm = makeStmt<stmt::ExprStmt>(commLoc,
                    std::make_unique<Expr>(Expr{expr::Assign{std::move(commExpr), TokenKind::Assign, std::move(value)}, commLoc}));
            }
            // Plain expression (e.g., <-ch)
            else {
                comm = makeStmt<stmt::ExprStmt>(commLoc, std::move(commExpr));
            }

            expect(TokenKind::Colon, "expected ':'");
        }

        // Parse body statements until next arm or }
        std::vector<StmtPtr> body;
        while (!check(TokenKind::RBrace) && !check(TokenKind::Eof)) {
            // Peek ahead: if this looks like a new select arm, stop
            // New arm: _ followed by :, or <-expr, or ident followed by comm ops
            if (check(TokenKind::Ident) && peek().text == "_" &&
                peekNext().kind == TokenKind::Colon) break;
            if (check(TokenKind::Arrow)) break;  // <-ch arm
            if (check(TokenKind::Ident) && (peekNext().kind == TokenKind::Arrow ||
                peekNext().kind == TokenKind::ColonAssign ||
                peekNext().kind == TokenKind::Comma ||
                peekNext().kind == TokenKind::Assign)) break;
            if (match(TokenKind::Semicolon)) continue;
            body.push_back(parseStmt());
        }

        cases.push_back(SelectCase{std::move(comm), std::move(body)});
    }

    expect(TokenKind::RBrace, "expected '}'");
    expectSemicolon();

    return makeStmt<stmt::Select>(location, std::move(cases));
}

StmtPtr Parser::parseSimpleStmt() {
    auto location = loc();
    auto expr_result = parseExpr();

    // Multi-value short decl or single short decl: x := expr or x, y := expr
    if (check(TokenKind::Comma) || check(TokenKind::ColonAssign)) {
        // Collect all names separated by commas
        std::vector<std::string> names;
        if (auto* ident = std::get_if<expr::Ident>(&expr_result->kind)) {
            names.push_back(ident->name);
        } else {
            diag_.error(location, "expected identifier on left side of ':='");
            names.push_back("_");
        }

        while (match(TokenKind::Comma)) {
            auto nextExpr = parseExpr();
            if (auto* ident = std::get_if<expr::Ident>(&nextExpr->kind)) {
                names.push_back(ident->name);
            } else {
                diag_.error(nextExpr->loc, "expected identifier on left side of ':='");
                names.push_back("_");
            }
        }

        if (match(TokenKind::ColonAssign)) {
            auto value = parseExpr();
            expectSemicolon();
            return makeStmt<stmt::ExprStmt>(location,
                std::make_unique<Expr>(Expr{expr::ShortDecl{std::move(names), std::move(value)}, location}));
        }

        // If we got commas but no :=, this is an error or a multi-value assignment
        // For now, treat as error
        if (names.size() > 1) {
            diag_.error(location, "expected ':=' after multiple identifiers");
        }
    }

    // Send: expr <- expr
    if (check(TokenKind::Arrow)) {
        advance();
        auto value = parseExpr();
        expectSemicolon();
        return makeStmt<stmt::Send>(location, std::move(expr_result), std::move(value));
    }

    // Assignment: expr = expr, expr += expr, etc.
    if (check(TokenKind::Assign) || check(TokenKind::PlusAssign) || check(TokenKind::MinusAssign) ||
        check(TokenKind::StarAssign) || check(TokenKind::SlashAssign) || check(TokenKind::PercentAssign)) {
        auto op = advance().kind;
        auto value = parseExpr();
        expectSemicolon();
        return makeStmt<stmt::ExprStmt>(location,
            std::make_unique<Expr>(Expr{expr::Assign{std::move(expr_result), op, std::move(value)}, location}));
    }

    // Increment / decrement: expr++ or expr--
    if (check(TokenKind::PlusPlus) || check(TokenKind::MinusMinus)) {
        auto op = advance().kind;
        expectSemicolon();
        return makeStmt<stmt::IncDec>(location, std::move(expr_result), op);
    }

    // Plain expression statement
    expectSemicolon();
    return makeStmt<stmt::ExprStmt>(location, std::move(expr_result));
}

// ─── Expressions ────────────────────────────────────────────────────

ExprPtr Parser::parseExpr() {
    return parseOr();
}

ExprPtr Parser::parseOr() {
    auto left = parseAnd();
    while (check(TokenKind::PipePipe)) {
        auto opLoc = loc();
        advance();
        auto right = parseAnd();
        left = std::make_unique<Expr>(Expr{
            expr::Binary{std::move(left), TokenKind::PipePipe, std::move(right)}, opLoc});
    }
    return left;
}

ExprPtr Parser::parseAnd() {
    auto left = parseComparison();
    while (check(TokenKind::AmpAmp)) {
        auto opLoc = loc();
        advance();
        auto right = parseComparison();
        left = std::make_unique<Expr>(Expr{
            expr::Binary{std::move(left), TokenKind::AmpAmp, std::move(right)}, opLoc});
    }
    return left;
}

ExprPtr Parser::parseComparison() {
    auto left = parseAddition();
    while (check(TokenKind::Equal) || check(TokenKind::NotEqual) ||
           check(TokenKind::Less) || check(TokenKind::Greater) ||
           check(TokenKind::LessEqual) || check(TokenKind::GreaterEqual)) {
        auto opLoc = loc();
        auto op = advance().kind;
        auto right = parseAddition();
        left = std::make_unique<Expr>(Expr{
            expr::Binary{std::move(left), op, std::move(right)}, opLoc});
    }
    return left;
}

ExprPtr Parser::parseAddition() {
    auto left = parseMultiplication();
    while (check(TokenKind::Plus) || check(TokenKind::Minus) ||
           check(TokenKind::Pipe) || check(TokenKind::Caret)) {
        auto opLoc = loc();
        auto op = advance().kind;
        auto right = parseMultiplication();
        left = std::make_unique<Expr>(Expr{
            expr::Binary{std::move(left), op, std::move(right)}, opLoc});
    }
    return left;
}

ExprPtr Parser::parseMultiplication() {
    auto left = parseUnary();
    while (check(TokenKind::Star) || check(TokenKind::Slash) || check(TokenKind::Percent) ||
           check(TokenKind::Ampersand) || check(TokenKind::ShiftLeft) || check(TokenKind::ShiftRight)) {
        auto opLoc = loc();
        auto op = advance().kind;
        auto right = parseUnary();
        left = std::make_unique<Expr>(Expr{
            expr::Binary{std::move(left), op, std::move(right)}, opLoc});
    }
    return left;
}

ExprPtr Parser::parseUnary() {
    // try expression: try callExpr
    if (check(TokenKind::Try)) {
        auto opLoc = loc(); advance();
        auto operand = parseUnary();
        return makeExpr<expr::Try>(opLoc, std::move(operand));
    }
    if (check(TokenKind::Minus) || check(TokenKind::Bang) ||
        check(TokenKind::Star) || check(TokenKind::Ampersand)) {
        auto opLoc = loc();
        auto op = advance().kind;
        auto operand = parseUnary();
        return std::make_unique<Expr>(Expr{expr::Unary{op, std::move(operand)}, opLoc});
    }
    // Channel receive: <-ch
    if (check(TokenKind::Arrow)) {
        auto opLoc = loc();
        advance();
        auto operand = parseUnary();
        return std::make_unique<Expr>(Expr{expr::Unary{TokenKind::Arrow, std::move(operand)}, opLoc});
    }
    return parsePostfix();
}

ExprPtr Parser::parsePostfix() {
    auto result = parsePrimary();

    while (true) {
        if (check(TokenKind::LParen)) {
            // Function call
            auto callLoc = loc();
            auto args = parseArgList();
            result = std::make_unique<Expr>(Expr{
                expr::Call{std::move(result), std::move(args)}, callLoc});
        } else if (check(TokenKind::Dot)) {
            auto dotLoc = loc();
            advance();
            // Type assertion: expr.(Type)
            if (match(TokenKind::LParen)) {
                auto type = parseType();
                expect(TokenKind::RParen, "expected ')'");
                result = std::make_unique<Expr>(Expr{
                    expr::TypeAssert{std::move(result), std::move(type)}, dotLoc});
            } else {
                // Member access: expr.member
                auto member = expect(TokenKind::Ident, "expected member name after '.'").text;
                result = std::make_unique<Expr>(Expr{
                    expr::Selector{std::move(result), member}, dotLoc});
            }
        } else if (check(TokenKind::LBracket)) {
            // Index or slice: expr[index] or expr[lo:hi]
            auto idxLoc = loc();
            advance();
            if (match(TokenKind::Colon)) {
                // expr[:hi]
                ExprPtr high;
                if (!check(TokenKind::RBracket)) {
                    high = parseExpr();
                }
                expect(TokenKind::RBracket, "expected ']'");
                ExprPtr low = std::make_unique<Expr>(Expr{expr::IntLit{"0"}, idxLoc});
                if (!high) high = std::make_unique<Expr>(Expr{expr::NilLit{}, idxLoc});
                result = std::make_unique<Expr>(Expr{
                    expr::SliceExpr{std::move(result), std::move(low), std::move(high)}, idxLoc});
            } else {
                auto index = parseExpr();
                if (match(TokenKind::Colon)) {
                    // expr[lo:hi]
                    ExprPtr high;
                    if (!check(TokenKind::RBracket)) {
                        high = parseExpr();
                    }
                    expect(TokenKind::RBracket, "expected ']'");
                    if (!high) high = std::make_unique<Expr>(Expr{expr::NilLit{}, idxLoc});
                    result = std::make_unique<Expr>(Expr{
                        expr::SliceExpr{std::move(result), std::move(index), std::move(high)}, idxLoc});
                } else {
                    expect(TokenKind::RBracket, "expected ']'");
                    result = std::make_unique<Expr>(Expr{
                        expr::Index{std::move(result), std::move(index)}, idxLoc});
                }
            }
        } else if (check(TokenKind::Less) &&
                   (std::holds_alternative<expr::Ident>(result->kind) ||
                    std::holds_alternative<expr::ScopeAccess>(result->kind)) &&
                   looksLikeTypeArgs()) {
            // Possible generic: Type<Args>{...} or Func<Args>(...)
            // Lookahead to confirm: parse type args, then check for { or (
            auto savedPos = pos_;
            bool savedPG = pendingGreater_;
            auto litLoc = result->loc;

            auto args = parseTypeArgs();
            if (check(TokenKind::LBrace) && compositeLitOk_) {
                // Generic composite literal: Type<Args>{...}
                TypeRefPtr base;
                if (auto* ident = std::get_if<expr::Ident>(&result->kind)) {
                    base = makeTypeRef<type_ref::Named>(litLoc, ident->name);
                } else if (auto* scope = std::get_if<expr::ScopeAccess>(&result->kind)) {
                    base = makeTypeRef<type_ref::Qualified>(litLoc, scope->scope, scope->member);
                }
                auto typeRef = makeTypeRef<type_ref::Generic>(litLoc, std::move(base), std::move(args));
                result = parseCompositeLitBody(std::move(typeRef), litLoc);
            } else if (check(TokenKind::LParen)) {
                // Generic function call: Func<Args>(...)
                // Type args are discarded from AST for now (Go infers them),
                // but we need to consume them. For codegen, we need to pass them.
                // Build the call with type args as an Index node? No — simplest:
                // just consume and proceed to call. Type args dropped for now.
                auto callArgs = parseArgList();
                result = std::make_unique<Expr>(Expr{
                    expr::Call{std::move(result), std::move(callArgs)}, litLoc});
            } else {
                // Not a generic expression — backtrack
                pos_ = savedPos;
                pendingGreater_ = savedPG;
                break;
            }
        } else if (check(TokenKind::LBrace) && compositeLitOk_) {
            // Composite literal: Type{...}
            // Only if the result looks like a type (Ident or ScopeAccess)
            bool isTypeLike = std::holds_alternative<expr::Ident>(result->kind) ||
                              std::holds_alternative<expr::ScopeAccess>(result->kind);
            if (isTypeLike) {
                auto litLoc = result->loc;
                TypeRefPtr typeRef;
                if (auto* identV = std::get_if<expr::Ident>(&result->kind)) {
                    typeRef = makeTypeRef<type_ref::Named>(litLoc, identV->name);
                } else if (auto* scope = std::get_if<expr::ScopeAccess>(&result->kind)) {
                    typeRef = makeTypeRef<type_ref::Qualified>(litLoc, scope->scope, scope->member);
                }
                result = parseCompositeLitBody(std::move(typeRef), litLoc);
            } else {
                break;
            }
        } else if (check(TokenKind::FatArrow)) {
            // Catch operator: expr => [const err] { body }
            auto catchLoc = loc();
            advance(); // consume =>
            expect(TokenKind::LBracket, "expected '[' after '=>'");
            bool isConst = match(TokenKind::Const);
            auto errVar = expect(TokenKind::Ident, "expected error variable name").text;
            expect(TokenKind::RBracket, "expected ']'");
            auto block = parseBlock();
            result = std::make_unique<Expr>(Expr{
                expr::Catch{std::move(result), errVar, isConst, std::move(block->stmts)}, catchLoc});
        } else if (check(TokenKind::Else)) {
            // Optional unwrap: expr else fallback
            auto elseLoc = loc(); advance();
            auto fallback = parseUnary();
            result = makeExpr<expr::Else>(elseLoc, std::move(result), std::move(fallback));
        } else {
            break;
        }
    }

    return result;
}

ExprPtr Parser::parsePrimary() {
    auto location = loc();

    // Parenthesized expression
    if (match(TokenKind::LParen)) {
        auto inner = parseExpr();
        expect(TokenKind::RParen, "expected ')'");
        return inner;
    }

    // .field shorthand → this.field
    if (check(TokenKind::Dot) && peekNext().kind == TokenKind::Ident) {
        advance(); // consume .
        auto member = advance().text;
        auto thisExpr = std::make_unique<Expr>(Expr{expr::This{}, location});
        return std::make_unique<Expr>(Expr{
            expr::Selector{std::move(thisExpr), member}, location});
    }

    // Closure: func(params) rettype { body }
    if (check(TokenKind::Func)) {
        return parseClosure();
    }

    // Slice/Array composite literal: []T{...} or [N]T{...}
    if (check(TokenKind::LBracket) && compositeLitOk_) {
        auto savedPos = pos_;
        auto typeRef = parseType();
        if (check(TokenKind::LBrace)) {
            return parseCompositeLitBody(std::move(typeRef), location);
        }
        // Backtrack — not a composite literal
        pos_ = savedPos;
    }

    // Map composite literal: map[K]V{...}
    if (check(TokenKind::Map) && compositeLitOk_) {
        auto savedPos = pos_;
        auto typeRef = parseType();
        if (check(TokenKind::LBrace)) {
            return parseCompositeLitBody(std::move(typeRef), location);
        }
        pos_ = savedPos;
    }

    // Literals
    if (check(TokenKind::IntLit)) {
        auto val = advance().text;
        return std::make_unique<Expr>(Expr{expr::IntLit{val}, location});
    }
    if (check(TokenKind::FloatLit)) {
        auto val = advance().text;
        return std::make_unique<Expr>(Expr{expr::FloatLit{val}, location});
    }
    if (check(TokenKind::StringLit)) {
        auto val = advance().text;
        return std::make_unique<Expr>(Expr{expr::StringLit{val}, location});
    }
    if (check(TokenKind::True)) {
        advance();
        return std::make_unique<Expr>(Expr{expr::BoolLit{true}, location});
    }
    if (check(TokenKind::False)) {
        advance();
        return std::make_unique<Expr>(Expr{expr::BoolLit{false}, location});
    }
    if (check(TokenKind::Nil)) {
        advance();
        return std::make_unique<Expr>(Expr{expr::NilLit{}, location});
    }
    if (check(TokenKind::This)) {
        advance();
        return std::make_unique<Expr>(Expr{expr::This{}, location});
    }

    // make(...) and other builtins - just parse as identifiers + calls
    if (check(TokenKind::Make) || check(TokenKind::Append) ||
        check(TokenKind::Len) || check(TokenKind::Cap)) {
        auto name = advance().text;
        return std::make_unique<Expr>(Expr{expr::Ident{name}, location});
    }

    // Identifier — possibly followed by :: for scope access
    if (check(TokenKind::Ident)) {
        auto name = advance().text;
        if (match(TokenKind::ColonColon)) {
            auto member = expect(TokenKind::Ident, "expected name after '::'").text;
            return std::make_unique<Expr>(Expr{expr::ScopeAccess{name, member}, location});
        }
        return std::make_unique<Expr>(Expr{expr::Ident{name}, location});
    }

    diag_.error(location, "expected expression, got '" + peek().text + "'");
    advance();
    return std::make_unique<Expr>(Expr{expr::NilLit{}, location});
}

ExprPtr Parser::parseCompositeLitBody(TypeRefPtr type, SourceLocation litLoc) {
    expect(TokenKind::LBrace, "expected '{'");
    std::vector<ExprPtr> elements;

    while (!check(TokenKind::RBrace) && !check(TokenKind::Eof)) {
        if (match(TokenKind::Semicolon)) continue;

        auto elemLoc = loc();
        auto expr_val = parseExpr();

        // Check for key: value
        if (match(TokenKind::Colon)) {
            auto value = parseExpr();
            elements.push_back(std::make_unique<Expr>(Expr{
                expr::KeyValue{std::move(expr_val), std::move(value)}, elemLoc}));
        } else {
            elements.push_back(std::move(expr_val));
        }

        if (!check(TokenKind::RBrace)) {
            // Allow comma or semicolon (newline) between elements
            if (!match(TokenKind::Comma)) {
                // Semicolon from newline is also acceptable
                match(TokenKind::Semicolon);
            }
        }
    }

    expect(TokenKind::RBrace, "expected '}'");
    return std::make_unique<Expr>(Expr{
        expr::CompositeLit{std::move(type), std::move(elements)}, litLoc});
}

ExprPtr Parser::parseClosure() {
    auto location = loc();
    expect(TokenKind::Func, "expected 'func'");
    auto params = parseParams();
    auto returns = parseReturnTypes();

    std::vector<StmtPtr> body;
    if (check(TokenKind::LBrace)) {
        auto block = parseBlock();
        body = std::move(block->stmts);
    }

    return std::make_unique<Expr>(Expr{
        expr::Closure{std::move(params), std::move(returns), std::move(body)}, location});
}

std::vector<ExprPtr> Parser::parseArgList() {
    expect(TokenKind::LParen, "expected '('");
    std::vector<ExprPtr> args;

    if (!check(TokenKind::RParen)) {
        args.push_back(parseExpr());
        while (match(TokenKind::Comma)) {
            args.push_back(parseExpr());
        }
    }

    expect(TokenKind::RParen, "expected ')'");
    return args;
}

// ─── Parameters ─────────────────────────────────────────────────────

std::vector<Param> Parser::parseParams() {
    expect(TokenKind::LParen, "expected '('");
    std::vector<Param> params;

    if (!check(TokenKind::RParen)) {
        auto paramLoc = loc();
        auto name = expect(TokenKind::Ident, "expected parameter name").text;
        auto type = parseType();
        params.push_back(Param{name, std::move(type), paramLoc});

        while (match(TokenKind::Comma)) {
            paramLoc = loc();
            name = expect(TokenKind::Ident, "expected parameter name").text;
            type = parseType();
            params.push_back(Param{name, std::move(type), paramLoc});
        }
    }

    expect(TokenKind::RParen, "expected ')'");
    return params;
}

} // namespace zo
