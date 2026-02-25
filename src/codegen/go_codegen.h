#pragma once

#include "codegen/codegen.h"
#include "common/diagnostics.h"

#include <sstream>
#include <string>
#include <unordered_map>

namespace zo {

class GoCodegen : public CodeGenerator {
public:
    explicit GoCodegen(DiagnosticEngine& diag);

    std::string generate(const File& file) override;

private:
    void emitDecl(const Decl& decl);
    void emitFunc(const decl::Func& fn, const std::string& receiver = "");
    void emitReturnTypes(const std::vector<TypeRefPtr>& returns);
    void emitStmt(const Stmt& stmt);
    void emitIf(const stmt::If& s);
    void emitBlock(const std::vector<StmtPtr>& stmts);
    void emitExpr(const Expr& expr);
    void emitTypeRef(const TypeRef& type);

    std::string mapType(const std::string& zoType) const;

    void indent();
    void dedent();
    void writeIndent();
    void write(const std::string& s);
    void writeln(const std::string& s);

    std::ostringstream out_;
    DiagnosticEngine& diag_;
    int indentLevel_ = 0;

    static const std::unordered_map<std::string, std::string> typeMap_;
};

} // namespace zo
