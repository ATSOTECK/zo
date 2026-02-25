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
    void emitFunc(const decl::Func& fn, const std::string& receiver = "", bool valueReceiver = false);
    void emitReturnTypes(const std::vector<TypeRefPtr>& returns);
    void emitStmt(const Stmt& stmt);
    void emitIf(const stmt::If& s);
    void emitBlock(const std::vector<StmtPtr>& stmts);
    void emitExpr(const Expr& expr);
    void emitTypeRef(const TypeRef& type);
    void emitEnum(const decl::Enum& e);
    void emitUnion(const decl::Union& u);
    void emitMatch(const stmt::Match& m);

    std::string mapType(const std::string& zoType) const;
    std::string resolveEnumVariant(const std::string& variant, const std::optional<std::string>& typeName) const;
    bool isUnionType(const std::string& name) const;
    std::string zeroValueForType(const TypeRef& type);
    bool containsTry(const Expr& expr);
    std::string typeRefToString(const TypeRef& type);

    void indent();
    void dedent();
    void writeIndent();
    void write(const std::string& s);
    void writeln(const std::string& s);

    std::ostringstream out_;
    DiagnosticEngine& diag_;
    int indentLevel_ = 0;

    // Result function tracking (T! returns)
    bool inResultFunc_ = false;
    const std::vector<TypeRefPtr>* currentReturns_ = nullptr;
    int tryCounter_ = 0;
    int elseCounter_ = 0;

    struct EnumInfo {
        std::string name;
        std::string underlying;  // Go type
        std::vector<std::string> variants;
    };
    std::vector<EnumInfo> enums_;

    struct UnionVariantInfo {
        std::string name;
        std::vector<std::string> fieldNames;  // capitalized field names
    };
    struct UnionInfo {
        std::string name;
        std::vector<UnionVariantInfo> variants;
    };
    std::vector<UnionInfo> unions_;

    static const std::unordered_map<std::string, std::string> typeMap_;
};

} // namespace zo
