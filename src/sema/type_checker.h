#pragma once

#include "common/diagnostics.h"
#include "parser/ast.h"
#include "sema/symbol_table.h"

namespace zo {

class TypeChecker {
public:
    explicit TypeChecker(DiagnosticEngine& diag);

    void check(const File& file);

private:
    void checkDecl(const Decl& decl);
    void checkFunc(const decl::Func& fn);
    void checkStmt(const Stmt& stmt);
    void checkExpr(const Expr& expr);
    void checkBlock(const std::vector<StmtPtr>& stmts);

    void registerBuiltins();
    void registerDecl(const Decl& decl);

    DiagnosticEngine& diag_;
    SymbolTable symbols_;
    std::string currentPackage_;

    struct EnumMeta {
        std::string name;
        std::vector<std::string> variants;
    };
    std::unordered_map<std::string, EnumMeta> enumTypes_;
    std::unordered_map<std::string, EnumMeta> unionTypes_;
};

} // namespace zo
