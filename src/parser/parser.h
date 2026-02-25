#pragma once

#include "common/diagnostics.h"
#include "lexer/token.h"
#include "parser/ast.h"

#include <vector>

namespace zo {

class Parser {
public:
    Parser(std::vector<Token> tokens, DiagnosticEngine& diag);

    File parseFile();

private:
    // Token stream
    const Token& peek() const;
    const Token& peekNext() const;
    const Token& advance();
    bool check(TokenKind kind) const;
    bool match(TokenKind kind);
    const Token& expect(TokenKind kind, const std::string& msg);
    void expectSemicolon();
    void synchronize();

    SourceLocation loc() const { return peek().loc; }

    // Top-level declarations
    Decl parseDecl();
    decl::Package parsePackage();
    decl::Import parseImport();
    decl::Func parseFunc();
    Decl parseTypeDecl();   // returns Struct, Interface, or TypeAlias
    decl::Struct parseStructBody(const std::string& name);
    decl::Interface parseInterface();
    decl::ImplBlock parseImpl();
    decl::Enum parseEnum();
    decl::Union parseUnion();

    // Statements (match)
    StmtPtr parseMatch();

    // Types
    TypeRefPtr parseType();
    TypeRefPtr parseBaseType();
    std::vector<TypeRefPtr> parseReturnTypes();

    // Statements
    StmtPtr parseStmt();
    std::unique_ptr<stmt::Block> parseBlock();
    StmtPtr parseReturn();
    StmtPtr parseVarDecl();
    StmtPtr parseConstDecl();
    StmtPtr parseIf();
    StmtPtr parseFor();
    StmtPtr parseSwitch();
    StmtPtr parseSelect();
    StmtPtr parseSimpleStmt();

    // Expressions
    ExprPtr parseExpr();
    ExprPtr parseOr();
    ExprPtr parseAnd();
    ExprPtr parseComparison();
    ExprPtr parseAddition();
    ExprPtr parseMultiplication();
    ExprPtr parseUnary();
    ExprPtr parsePostfix();
    ExprPtr parsePrimary();
    ExprPtr parseCompositeLitBody(TypeRefPtr type, SourceLocation litLoc);
    std::vector<ExprPtr> parseArgList();
    ExprPtr parseClosure();

    // Params
    std::vector<Param> parseParams();

    std::vector<Token> tokens_;
    DiagnosticEngine& diag_;
    size_t pos_ = 0;
    bool compositeLitOk_ = true;
};

} // namespace zo
