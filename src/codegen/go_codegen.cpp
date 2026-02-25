#include "codegen/go_codegen.h"

#include <variant>

namespace zo {

const std::unordered_map<std::string, std::string> GoCodegen::typeMap_ = {
    {"s8",     "int8"},
    {"s16",    "int16"},
    {"s32",    "int32"},
    {"s64",    "int64"},
    {"u8",     "uint8"},
    {"u16",    "uint16"},
    {"u32",    "uint32"},
    {"u64",    "uint64"},
    {"f32",    "float32"},
    {"f64",    "float64"},
    {"int",    "int"},
    {"uint",   "uint"},
    {"string", "string"},
    {"bool",   "bool"},
    {"byte",   "byte"},
};

GoCodegen::GoCodegen(DiagnosticEngine& diag) : diag_(diag) {}

void GoCodegen::indent() { ++indentLevel_; }
void GoCodegen::dedent() { if (indentLevel_ > 0) --indentLevel_; }
void GoCodegen::writeIndent() { for (int i = 0; i < indentLevel_; ++i) out_ << "\t"; }
void GoCodegen::write(const std::string& s) { out_ << s; }
void GoCodegen::writeln(const std::string& s) { writeIndent(); out_ << s << "\n"; }

std::string GoCodegen::mapType(const std::string& zoType) const {
    auto it = typeMap_.find(zoType);
    if (it != typeMap_.end()) return it->second;
    return zoType;
}

std::string GoCodegen::generate(const File& file) {
    out_.str("");
    out_.clear();
    indentLevel_ = 0;
    enums_.clear();
    unions_.clear();

    // Pre-scan for enum and union declarations to enable shorthand resolution
    for (const auto& decl : file.decls) {
        if (auto* e = std::get_if<decl::Enum>(&decl.kind)) {
            EnumInfo info;
            info.name = e->name;
            info.underlying = "int";
            if (e->underlying_type.has_value()) {
                // Extract underlying type name
                if (auto* named = std::get_if<type_ref::Named>(&(*e->underlying_type)->kind)) {
                    info.underlying = mapType(named->name);
                }
            }
            for (const auto& v : e->variants) {
                info.variants.push_back(v.name);
            }
            enums_.push_back(std::move(info));
        } else if (auto* u = std::get_if<decl::Union>(&decl.kind)) {
            UnionInfo info;
            info.name = u->name;
            for (const auto& v : u->variants) {
                UnionVariantInfo vi;
                vi.name = v.name;
                for (const auto& f : v.fields) {
                    std::string capName = f.name;
                    if (!capName.empty()) capName[0] = std::toupper(capName[0]);
                    vi.fieldNames.push_back(capName);
                }
                info.variants.push_back(std::move(vi));
            }
            unions_.push_back(std::move(info));
        }
    }

    for (const auto& decl : file.decls) {
        emitDecl(decl);
    }

    return out_.str();
}

std::string GoCodegen::resolveEnumVariant(const std::string& variant,
                                           const std::optional<std::string>& typeName) const {
    if (typeName.has_value()) {
        return *typeName + variant;
    }
    // Search enums for the variant
    for (const auto& e : enums_) {
        for (const auto& v : e.variants) {
            if (v == variant) return e.name + variant;
        }
    }
    // Search unions for the variant
    for (const auto& u : unions_) {
        for (const auto& v : u.variants) {
            if (v.name == variant) return u.name + variant;
        }
    }
    return variant;  // fallback
}

bool GoCodegen::isUnionType(const std::string& name) const {
    for (const auto& u : unions_) {
        if (u.name == name) return true;
    }
    return false;
}

void GoCodegen::emitTypeParams(const std::vector<TypeParam>& params) {
    if (params.empty()) return;
    write("[");
    for (size_t i = 0; i < params.size(); ++i) {
        if (i > 0) write(", ");
        write(params[i].name + " ");
        if (params[i].constraint.has_value()) {
            write(params[i].constraint.value());
        } else {
            write("any");
        }
    }
    write("]");
}

void GoCodegen::emitDecl(const Decl& decl) {
    std::visit([this](const auto& d) {
        using T = std::decay_t<decltype(d)>;

        if constexpr (std::is_same_v<T, decl::Package>) {
            writeln("package " + d.name);
            write("\n");
        }
        else if constexpr (std::is_same_v<T, decl::Import>) {
            std::string goPath = d.name;
            auto it = importMap_.find(d.name);
            if (it != importMap_.end()) goPath = it->second;

            if (d.alias.has_value()) {
                writeln("import " + d.alias.value() + " \"" + goPath + "\"");
            } else {
                std::string lastSeg = goPath.substr(goPath.rfind('/') + 1);
                if (lastSeg != d.name) {
                    writeln("import " + d.name + " \"" + goPath + "\"");
                } else {
                    writeln("import \"" + goPath + "\"");
                }
            }
            write("\n");
        }
        else if constexpr (std::is_same_v<T, decl::Func>) {
            emitFunc(d);
            write("\n");
        }
        else if constexpr (std::is_same_v<T, decl::Struct>) {
            writeIndent();
            write("type " + d.name);
            emitTypeParams(d.type_params);
            write(" struct {\n");
            indent();
            for (const auto& field : d.fields) {
                writeIndent();
                write(field.name + " ");
                emitTypeRef(*field.type);
                write("\n");
            }
            dedent();
            writeln("}");
            write("\n");
        }
        else if constexpr (std::is_same_v<T, decl::Interface>) {
            writeln("type " + d.name + " interface {");
            indent();
            for (const auto& method : d.methods) {
                writeIndent();
                write(method.name + "(");
                for (size_t i = 0; i < method.params.size(); ++i) {
                    if (i > 0) write(", ");
                    write(method.params[i].name + " ");
                    emitTypeRef(*method.params[i].type);
                }
                write(")");
                emitReturnTypes(method.returns);
                write("\n");
            }
            dedent();
            writeln("}");
            write("\n");
        }
        else if constexpr (std::is_same_v<T, decl::ImplBlock>) {
            // Check if target is an enum (use value receiver)
            bool isEnum = false;
            for (const auto& e : enums_) {
                if (e.name == d.target) { isEnum = true; break; }
            }
            // Build receiver string with type args for generics
            std::string receiver = d.target;
            if (!d.target_type_args.empty()) {
                receiver += "[";
                for (size_t i = 0; i < d.target_type_args.size(); ++i) {
                    if (i > 0) receiver += ", ";
                    receiver += d.target_type_args[i];
                }
                receiver += "]";
            }
            for (const auto& method : d.methods) {
                // Emit impl-level type params on each method
                if (!d.type_params.empty()) {
                    // For generic impl blocks, we emit type params on the receiver
                    // The method itself doesn't get separate type params from the impl
                }
                emitFunc(method, receiver, isEnum);
                write("\n");
            }
            if (d.interface_name.has_value()) {
                writeln("var _ " + d.interface_name.value() + " = (*" + d.target + ")(nil)");
                write("\n");
            }
        }
        else if constexpr (std::is_same_v<T, decl::TypeAlias>) {
            writeIndent();
            write("type " + d.name + " ");
            emitTypeRef(*d.type);
            write("\n\n");
        }
        else if constexpr (std::is_same_v<T, decl::Enum>) {
            emitEnum(d);
        }
        else if constexpr (std::is_same_v<T, decl::Union>) {
            emitUnion(d);
        }
        else if constexpr (std::is_same_v<T, decl::Constraint>) {
            writeIndent();
            write("type " + d.name + " interface { ");
            for (size_t i = 0; i < d.types.size(); ++i) {
                if (i > 0) write(" | ");
                emitTypeRef(*d.types[i]);
            }
            write(" }\n\n");
        }
    }, decl.kind);
}

void GoCodegen::emitFunc(const decl::Func& fn, const std::string& receiver, bool valueReceiver) {
    // Save/restore result-function state
    bool prevInResult = inResultFunc_;
    const std::vector<TypeRefPtr>* prevReturns = currentReturns_;
    int prevTry = tryCounter_;
    int prevElse = elseCounter_;

    // Detect if the function returns T! (Result type)
    inResultFunc_ = false;
    currentReturns_ = &fn.returns;
    tryCounter_ = 0;
    elseCounter_ = 0;
    for (const auto& ret : fn.returns) {
        if (std::holds_alternative<type_ref::Result>(ret->kind)) {
            inResultFunc_ = true;
            break;
        }
    }

    writeIndent();
    write("func ");

    if (!receiver.empty()) {
        if (valueReceiver) {
            write("(this " + receiver + ") ");
        } else {
            write("(this *" + receiver + ") ");
        }
    }

    write(fn.name);
    emitTypeParams(fn.type_params);
    write("(");
    for (size_t i = 0; i < fn.params.size(); ++i) {
        if (i > 0) write(", ");
        write(fn.params[i].name + " ");
        emitTypeRef(*fn.params[i].type);
    }
    write(")");

    emitReturnTypes(fn.returns);

    if (fn.body.empty() && fn.returns.empty()) {
        write("\n");
        inResultFunc_ = prevInResult;
        currentReturns_ = prevReturns;
        tryCounter_ = prevTry;
        elseCounter_ = prevElse;
        return;
    }

    write(" {\n");
    indent();
    for (const auto& stmt : fn.body) {
        emitStmt(*stmt);
    }
    dedent();
    writeln("}");

    // Restore
    inResultFunc_ = prevInResult;
    currentReturns_ = prevReturns;
    tryCounter_ = prevTry;
    elseCounter_ = prevElse;
}

void GoCodegen::emitReturnTypes(const std::vector<TypeRefPtr>& returns) {
    if (returns.empty()) return;
    if (returns.size() == 1) {
        write(" ");
        emitTypeRef(*returns[0]);
    } else {
        write(" (");
        for (size_t i = 0; i < returns.size(); ++i) {
            if (i > 0) write(", ");
            emitTypeRef(*returns[i]);
        }
        write(")");
    }
}

void GoCodegen::emitStmt(const Stmt& stmt) {
    std::visit([this](const auto& s) {
        using T = std::decay_t<decltype(s)>;

        if constexpr (std::is_same_v<T, stmt::ExprStmt>) {
            // Handle ShortDecl with try: x := try foo()
            if (auto* sd = std::get_if<expr::ShortDecl>(&s.expr->kind)) {
                if (sd->value && containsTry(*sd->value)) {
                    auto* tryExpr = std::get_if<expr::Try>(&sd->value->kind);
                    if (tryExpr) {
                        int n = tryCounter_++;
                        std::string errVar = "__err" + std::to_string(n);
                        // x, __errN := operand
                        writeIndent();
                        for (size_t i = 0; i < sd->names.size(); ++i) {
                            if (i > 0) write(", ");
                            write(sd->names[i]);
                        }
                        write(", " + errVar + " := ");
                        emitExpr(*tryExpr->operand);
                        write("\n");
                        // if __errN != nil { return zero, __errN }
                        writeln("if " + errVar + " != nil {");
                        indent();
                        writeIndent();
                        write("return ");
                        if (currentReturns_) {
                            for (size_t i = 0; i < currentReturns_->size(); ++i) {
                                if (i > 0) write(", ");
                                auto& ret = (*currentReturns_)[i];
                                if (std::holds_alternative<type_ref::Result>(ret->kind)) {
                                    auto& inner = std::get<type_ref::Result>(ret->kind).inner;
                                    write(zeroValueForType(*inner) + ", " + errVar);
                                } else {
                                    write(zeroValueForType(*ret));
                                }
                            }
                        }
                        write("\n");
                        dedent();
                        writeln("}");
                        return;
                    }
                }
            }
            // Handle bare try: try foo()
            if (containsTry(*s.expr)) {
                auto* tryExpr = std::get_if<expr::Try>(&s.expr->kind);
                if (tryExpr) {
                    int n = tryCounter_++;
                    std::string errVar = "__err" + std::to_string(n);
                    writeIndent();
                    write("_, " + errVar + " := ");
                    emitExpr(*tryExpr->operand);
                    write("\n");
                    writeln("if " + errVar + " != nil {");
                    indent();
                    writeIndent();
                    write("return ");
                    if (currentReturns_) {
                        for (size_t i = 0; i < currentReturns_->size(); ++i) {
                            if (i > 0) write(", ");
                            auto& ret = (*currentReturns_)[i];
                            if (std::holds_alternative<type_ref::Result>(ret->kind)) {
                                auto& inner = std::get<type_ref::Result>(ret->kind).inner;
                                write(zeroValueForType(*inner) + ", " + errVar);
                            } else {
                                write(zeroValueForType(*ret));
                            }
                        }
                    }
                    write("\n");
                    dedent();
                    writeln("}");
                    return;
                }
            }
            // Handle ShortDecl with else: x := getOpt() else default
            if (auto* sd = std::get_if<expr::ShortDecl>(&s.expr->kind)) {
                if (sd->value && std::holds_alternative<expr::Else>(sd->value->kind)) {
                    auto& elseExpr = std::get<expr::Else>(sd->value->kind);
                    int n = elseCounter_++;
                    std::string optVar = "__opt" + std::to_string(n);
                    // __optN := value
                    writeIndent();
                    write(optVar + " := ");
                    emitExpr(*elseExpr.value);
                    write("\n");
                    // x := fallback
                    writeIndent();
                    write(sd->names[0] + " := ");
                    emitExpr(*elseExpr.fallback);
                    write("\n");
                    // if __optN != nil { x = *__optN }
                    writeln("if " + optVar + " != nil {");
                    indent();
                    writeln(sd->names[0] + " = *" + optVar);
                    dedent();
                    writeln("}");
                    return;
                }
            }
            writeIndent();
            emitExpr(*s.expr);
            write("\n");
        }
        else if constexpr (std::is_same_v<T, stmt::Return>) {
            if (inResultFunc_) {
                // Check if any return value contains try
                bool hasTry = false;
                for (const auto& val : s.values) {
                    if (containsTry(*val)) { hasTry = true; break; }
                }
                if (hasTry && s.values.size() == 1) {
                    // return try foo() → extract try, then return val, nil
                    auto* tryExpr = std::get_if<expr::Try>(&s.values[0]->kind);
                    if (tryExpr) {
                        int n = tryCounter_++;
                        std::string tryVar = "__try" + std::to_string(n);
                        std::string errVar = "__err" + std::to_string(n);
                        writeIndent();
                        write(tryVar + ", " + errVar + " := ");
                        emitExpr(*tryExpr->operand);
                        write("\n");
                        writeln("if " + errVar + " != nil {");
                        indent();
                        writeIndent();
                        write("return ");
                        if (currentReturns_) {
                            for (size_t i = 0; i < currentReturns_->size(); ++i) {
                                if (i > 0) write(", ");
                                auto& ret = (*currentReturns_)[i];
                                if (std::holds_alternative<type_ref::Result>(ret->kind)) {
                                    auto& inner = std::get<type_ref::Result>(ret->kind).inner;
                                    write(zeroValueForType(*inner) + ", " + errVar);
                                } else {
                                    write(zeroValueForType(*ret));
                                }
                            }
                        }
                        write("\n");
                        dedent();
                        writeln("}");
                        writeIndent();
                        write("return " + tryVar + ", nil\n");
                        return;
                    }
                }
                // Auto-append nil: return val → return val, nil
                writeIndent();
                write("return");
                for (size_t i = 0; i < s.values.size(); ++i) {
                    write(i == 0 ? " " : ", ");
                    emitExpr(*s.values[i]);
                }
                write(", nil\n");
            } else {
                writeIndent();
                write("return");
                for (size_t i = 0; i < s.values.size(); ++i) {
                    write(i == 0 ? " " : ", ");
                    emitExpr(*s.values[i]);
                }
                write("\n");
            }
        }
        else if constexpr (std::is_same_v<T, stmt::VarDecl>) {
            // Handle var x T = getOpt() else default
            if (s.init.has_value() && std::holds_alternative<expr::Else>((*s.init)->kind)) {
                auto& elseExpr = std::get<expr::Else>((*s.init)->kind);
                int n = elseCounter_++;
                std::string optVar = "__opt" + std::to_string(n);
                writeIndent();
                write(optVar + " := ");
                emitExpr(*elseExpr.value);
                write("\n");
                writeIndent();
                if (s.type.has_value()) {
                    write("var " + s.name + " ");
                    emitTypeRef(**s.type);
                    write(" = ");
                } else {
                    write(s.name + " := ");
                }
                emitExpr(*elseExpr.fallback);
                write("\n");
                writeln("if " + optVar + " != nil {");
                indent();
                writeln(s.name + " = *" + optVar);
                dedent();
                writeln("}");
            } else {
                writeIndent();
                if (s.init.has_value()) {
                    if (s.type.has_value()) {
                        write("var " + s.name + " ");
                        emitTypeRef(**s.type);
                        write(" = ");
                        emitExpr(**s.init);
                    } else {
                        write("var " + s.name + " = ");
                        emitExpr(**s.init);
                    }
                } else if (s.type.has_value()) {
                    write("var " + s.name + " ");
                    emitTypeRef(**s.type);
                }
                write("\n");
            }
        }
        else if constexpr (std::is_same_v<T, stmt::ConstDecl>) {
            writeIndent();
            write("const " + s.name);
            if (s.type.has_value()) {
                write(" ");
                emitTypeRef(**s.type);
            }
            write(" = ");
            emitExpr(*s.init);
            write("\n");
        }
        else if constexpr (std::is_same_v<T, stmt::Block>) {
            writeln("{");
            indent();
            for (const auto& inner : s.stmts) {
                emitStmt(*inner);
            }
            dedent();
            writeln("}");
        }
        else if constexpr (std::is_same_v<T, stmt::If>) {
            writeIndent();
            emitIf(s);
        }
        else if constexpr (std::is_same_v<T, stmt::For>) {
            writeIndent();
            bool hasInit = s.init.has_value();
            bool hasCond = s.cond.has_value();
            bool hasPost = s.post.has_value();

            if (!hasInit && !hasCond && !hasPost) {
                write("for {\n");
            } else if (!hasInit && hasCond && !hasPost) {
                write("for ");
                emitExpr(**s.cond);
                write(" {\n");
            } else {
                write("for ");
                if (hasInit) {
                    // Emit init without newline
                    const auto& initStmt = **s.init;
                    if (auto* es = std::get_if<stmt::ExprStmt>(&initStmt.kind)) {
                        emitExpr(*es->expr);
                    }
                }
                write("; ");
                if (hasCond) emitExpr(**s.cond);
                write("; ");
                if (hasPost) {
                    const auto& postStmt = **s.post;
                    if (auto* es = std::get_if<stmt::ExprStmt>(&postStmt.kind)) {
                        emitExpr(*es->expr);
                    } else if (auto* id = std::get_if<stmt::IncDec>(&postStmt.kind)) {
                        emitExpr(*id->operand);
                        write(id->op == TokenKind::PlusPlus ? "++" : "--");
                    }
                }
                write(" {\n");
            }
            indent();
            for (const auto& inner : s.body->stmts) {
                emitStmt(*inner);
            }
            dedent();
            writeln("}");
        }
        else if constexpr (std::is_same_v<T, stmt::ForRange>) {
            writeIndent();
            write("for " + s.key);
            if (s.value.has_value()) {
                write(", " + s.value.value());
            }
            write(s.define ? " := range " : " = range ");
            emitExpr(*s.iterable);
            write(" {\n");
            indent();
            for (const auto& inner : s.body->stmts) {
                emitStmt(*inner);
            }
            dedent();
            writeln("}");
        }
        else if constexpr (std::is_same_v<T, stmt::Match>) {
            emitMatch(s);
        }
        else if constexpr (std::is_same_v<T, stmt::Select>) {
            writeln("select {");
            for (const auto& c : s.cases) {
                writeIndent();
                if (!c.comm.has_value()) {
                    write("default:\n");
                } else {
                    write("case ");
                    const auto& commStmt = **c.comm;
                    if (auto* es = std::get_if<stmt::ExprStmt>(&commStmt.kind)) {
                        emitExpr(*es->expr);
                    } else if (auto* send = std::get_if<stmt::Send>(&commStmt.kind)) {
                        emitExpr(*send->channel);
                        write(" <- ");
                        emitExpr(*send->value);
                    }
                    write(":\n");
                }
                indent();
                for (const auto& bodyStmt : c.body) {
                    emitStmt(*bodyStmt);
                }
                dedent();
            }
            writeln("}");
        }
        else if constexpr (std::is_same_v<T, stmt::Send>) {
            writeIndent();
            emitExpr(*s.channel);
            write(" <- ");
            emitExpr(*s.value);
            write("\n");
        }
        else if constexpr (std::is_same_v<T, stmt::GoStmt>) {
            writeIndent();
            write("go ");
            emitExpr(*s.call);
            write("\n");
        }
        else if constexpr (std::is_same_v<T, stmt::DeferStmt>) {
            writeIndent();
            write("defer ");
            emitExpr(*s.call);
            write("\n");
        }
        else if constexpr (std::is_same_v<T, stmt::IncDec>) {
            writeIndent();
            emitExpr(*s.operand);
            write(s.op == TokenKind::PlusPlus ? "++" : "--");
            write("\n");
        }
        else if constexpr (std::is_same_v<T, stmt::Break>) {
            writeln("break");
        }
        else if constexpr (std::is_same_v<T, stmt::Continue>) {
            writeln("continue");
        }
        else if constexpr (std::is_same_v<T, stmt::Fallthrough>) {
            writeln("fallthrough");
        }
    }, stmt.kind);
}

void GoCodegen::emitIf(const stmt::If& s) {
    write("if ");
    emitExpr(*s.cond);
    write(" {\n");
    indent();
    for (const auto& inner : s.then_block->stmts) {
        emitStmt(*inner);
    }
    dedent();
    writeIndent();
    if (s.else_block.has_value()) {
        write("} else ");
        const auto& elseStmt = **s.else_block;
        if (std::holds_alternative<stmt::If>(elseStmt.kind)) {
            emitIf(std::get<stmt::If>(elseStmt.kind));
        } else {
            write("{\n");
            indent();
            const auto& block = std::get<stmt::Block>(elseStmt.kind);
            for (const auto& inner : block.stmts) {
                emitStmt(*inner);
            }
            dedent();
            writeIndent();
            write("}\n");
        }
    } else {
        write("}\n");
    }
}

void GoCodegen::emitBlock(const std::vector<StmtPtr>& stmts) {
    writeln("{");
    indent();
    for (const auto& s : stmts) {
        emitStmt(*s);
    }
    dedent();
    writeln("}");
}

void GoCodegen::emitExpr(const Expr& expr) {
    std::visit([this](const auto& e) {
        using T = std::decay_t<decltype(e)>;

        if constexpr (std::is_same_v<T, expr::Ident>) {
            write(e.name);
        }
        else if constexpr (std::is_same_v<T, expr::StringLit>) {
            write("\"");
            for (char c : e.value) {
                switch (c) {
                    case '\n': write("\\n"); break;
                    case '\t': write("\\t"); break;
                    case '\r': write("\\r"); break;
                    case '\\': write("\\\\"); break;
                    case '"':  write("\\\""); break;
                    case '\0': write("\\x00"); break;
                    default:   write(std::string(1, c)); break;
                }
            }
            write("\"");
        }
        else if constexpr (std::is_same_v<T, expr::IntLit>) {
            write(e.value);
        }
        else if constexpr (std::is_same_v<T, expr::FloatLit>) {
            write(e.value);
        }
        else if constexpr (std::is_same_v<T, expr::BoolLit>) {
            write(e.value ? "true" : "false");
        }
        else if constexpr (std::is_same_v<T, expr::NilLit>) {
            write("nil");
        }
        else if constexpr (std::is_same_v<T, expr::This>) {
            write("this");
        }
        else if constexpr (std::is_same_v<T, expr::ScopeAccess>) {
            write(e.scope + "." + e.member);
        }
        else if constexpr (std::is_same_v<T, expr::Selector>) {
            emitExpr(*e.object);
            write("." + e.member);
        }
        else if constexpr (std::is_same_v<T, expr::Call>) {
            emitExpr(*e.callee);
            write("(");
            for (size_t i = 0; i < e.args.size(); ++i) {
                if (i > 0) write(", ");
                emitExpr(*e.args[i]);
            }
            write(")");
        }
        else if constexpr (std::is_same_v<T, expr::Index>) {
            emitExpr(*e.object);
            write("[");
            emitExpr(*e.index);
            write("]");
        }
        else if constexpr (std::is_same_v<T, expr::Unary>) {
            if (e.op == TokenKind::Arrow) {
                write("<-");
            } else {
                write(std::string(tokenKindName(e.op)));
            }
            emitExpr(*e.operand);
        }
        else if constexpr (std::is_same_v<T, expr::Binary>) {
            emitExpr(*e.left);
            write(" " + std::string(tokenKindName(e.op)) + " ");
            emitExpr(*e.right);
        }
        else if constexpr (std::is_same_v<T, expr::Assign>) {
            emitExpr(*e.target);
            write(" " + std::string(tokenKindName(e.op)) + " ");
            emitExpr(*e.value);
        }
        else if constexpr (std::is_same_v<T, expr::ShortDecl>) {
            for (size_t i = 0; i < e.names.size(); ++i) {
                if (i > 0) write(", ");
                write(e.names[i]);
            }
            write(" := ");
            emitExpr(*e.value);
        }
        else if constexpr (std::is_same_v<T, expr::CompositeLit>) {
            emitTypeRef(*e.type);
            write("{");
            for (size_t i = 0; i < e.elements.size(); ++i) {
                if (i > 0) write(", ");
                emitExpr(*e.elements[i]);
            }
            write("}");
        }
        else if constexpr (std::is_same_v<T, expr::KeyValue>) {
            emitExpr(*e.key);
            write(": ");
            emitExpr(*e.value);
        }
        else if constexpr (std::is_same_v<T, expr::Closure>) {
            write("func(");
            for (size_t i = 0; i < e.params.size(); ++i) {
                if (i > 0) write(", ");
                write(e.params[i].name + " ");
                emitTypeRef(*e.params[i].type);
            }
            write(")");
            // Return types
            if (!e.returns.empty()) {
                if (e.returns.size() == 1) {
                    write(" ");
                    emitTypeRef(*e.returns[0]);
                } else {
                    write(" (");
                    for (size_t i = 0; i < e.returns.size(); ++i) {
                        if (i > 0) write(", ");
                        emitTypeRef(*e.returns[i]);
                    }
                    write(")");
                }
            }
            write(" {\n");
            indent();
            for (const auto& stmt : e.body) {
                emitStmt(*stmt);
            }
            dedent();
            writeIndent();
            write("}");
        }
        else if constexpr (std::is_same_v<T, expr::SliceExpr>) {
            emitExpr(*e.object);
            write("[");
            emitExpr(*e.low);
            write(":");
            if (!std::holds_alternative<expr::NilLit>(e.high->kind)) {
                emitExpr(*e.high);
            }
            write("]");
        }
        else if constexpr (std::is_same_v<T, expr::TypeAssert>) {
            emitExpr(*e.object);
            write(".(");
            emitTypeRef(*e.type);
            write(")");
        }
        else if constexpr (std::is_same_v<T, expr::EnumVariant>) {
            write(resolveEnumVariant(e.variant, e.type_name));
        }
        else if constexpr (std::is_same_v<T, expr::UnionVariant>) {
            write(resolveEnumVariant(e.variant, e.type_name));
        }
        else if constexpr (std::is_same_v<T, expr::Try>) {
            // Fallback: try in expression position emits just the operand
            // (statement-level try is handled in emitStmt)
            emitExpr(*e.operand);
        }
        else if constexpr (std::is_same_v<T, expr::Else>) {
            // Fallback: else in expression position emits just the value
            // (statement-level else is handled in emitStmt)
            emitExpr(*e.value);
        }
    }, expr.kind);
}

void GoCodegen::emitEnum(const decl::Enum& e) {
    // Determine underlying Go type
    std::string goType = "int";
    if (e.underlying_type.has_value()) {
        if (auto* named = std::get_if<type_ref::Named>(&(*e.underlying_type)->kind)) {
            goType = mapType(named->name);
        }
    }

    writeln("type " + e.name + " " + goType);

    // Check if any variant has an explicit value
    bool hasExplicitValues = false;
    for (const auto& v : e.variants) {
        if (v.value.has_value()) {
            hasExplicitValues = true;
            break;
        }
    }

    writeln("const (");
    indent();
    for (size_t i = 0; i < e.variants.size(); ++i) {
        const auto& v = e.variants[i];
        writeIndent();
        if (hasExplicitValues) {
            write(e.name + v.name + " " + e.name);
            if (v.value.has_value()) {
                write(" = ");
                emitExpr(**v.value);
            }
        } else {
            if (i == 0) {
                write(e.name + v.name + " " + e.name + " = iota");
            } else {
                write(e.name + v.name);
            }
        }
        write("\n");
    }
    dedent();
    writeln(")");
    write("\n");
}

void GoCodegen::emitUnion(const decl::Union& u) {
    // Emit interface
    writeln("type " + u.name + " interface{ is" + u.name + "() }");

    // Emit variant structs + marker methods
    for (const auto& v : u.variants) {
        writeIndent();
        write("type " + u.name + v.name + " struct{");
        if (!v.fields.empty()) {
            write(" ");
            for (size_t i = 0; i < v.fields.size(); ++i) {
                if (i > 0) write("; ");
                // Capitalize field name for export
                std::string fieldName = v.fields[i].name;
                if (!fieldName.empty()) {
                    fieldName[0] = std::toupper(fieldName[0]);
                }
                write(fieldName + " ");
                emitTypeRef(*v.fields[i].type);
            }
            write(" ");
        }
        write("}\n");
        writeln("func (" + u.name + v.name + ") is" + u.name + "() {}");
    }
    write("\n");
}

void GoCodegen::emitMatch(const stmt::Match& m) {
    // Determine if this is a union type switch or an enum value switch
    // We check the patterns: if any arm has UnionVariant patterns, use type switch
    bool isUnionMatch = false;
    for (const auto& arm : m.arms) {
        for (const auto& pat : arm.patterns) {
            if (std::holds_alternative<expr::UnionVariant>(pat->kind)) {
                isUnionMatch = true;
                break;
            }
            // Also check if an EnumVariant resolves to a union
            if (auto* ev = std::get_if<expr::EnumVariant>(&pat->kind)) {
                if (ev->type_name.has_value() && isUnionType(*ev->type_name)) {
                    isUnionMatch = true;
                } else if (!ev->type_name.has_value()) {
                    // Search unions
                    for (const auto& u : unions_) {
                        for (const auto& v : u.variants) {
                            if (v.name == ev->variant) {
                                isUnionMatch = true;
                                break;
                            }
                        }
                        if (isUnionMatch) break;
                    }
                }
            }
        }
        if (isUnionMatch) break;
    }

    writeIndent();
    if (isUnionMatch) {
        write("switch __v := ");
        emitExpr(*m.value);
        write(".(type) {\n");
    } else {
        write("switch ");
        emitExpr(*m.value);
        write(" {\n");
    }

    for (const auto& arm : m.arms) {
        writeIndent();

        // Check for wildcard
        bool isDefault = false;
        for (const auto& pat : arm.patterns) {
            if (auto* ident = std::get_if<expr::Ident>(&pat->kind)) {
                if (ident->name == "_") {
                    isDefault = true;
                    break;
                }
            }
        }

        if (isDefault) {
            write("default:\n");
        } else {
            write("case ");
            for (size_t i = 0; i < arm.patterns.size(); ++i) {
                if (i > 0) write(", ");
                const auto& pat = *arm.patterns[i];
                if (auto* ev = std::get_if<expr::EnumVariant>(&pat.kind)) {
                    write(resolveEnumVariant(ev->variant, ev->type_name));
                } else if (auto* uv = std::get_if<expr::UnionVariant>(&pat.kind)) {
                    write(resolveEnumVariant(uv->variant, uv->type_name));
                } else {
                    emitExpr(pat);
                }
            }
            write(":\n");
        }

        indent();

        // Emit destructuring bindings for union match
        if (isUnionMatch && !isDefault) {
            for (const auto& pat : arm.patterns) {
                if (auto* uv = std::get_if<expr::UnionVariant>(&pat->kind)) {
                    if (!uv->bindings.empty()) {
                        // Find the union variant to get field names
                        for (const auto& u : unions_) {
                            for (const auto& v : u.variants) {
                                if (v.name == uv->variant) {
                                    for (size_t i = 0; i < uv->bindings.size() && i < v.fieldNames.size(); ++i) {
                                        writeIndent();
                                        write(uv->bindings[i] + " := __v." + v.fieldNames[i] + "\n");
                                    }
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }

        for (const auto& s : arm.body) {
            emitStmt(*s);
        }
        dedent();
    }

    writeln("}");
}

void GoCodegen::emitTypeRef(const TypeRef& type) {
    std::visit([this](const auto& t) {
        using T = std::decay_t<decltype(t)>;

        if constexpr (std::is_same_v<T, type_ref::Named>) {
            write(mapType(t.name));
        }
        else if constexpr (std::is_same_v<T, type_ref::Qualified>) {
            write(t.pkg + "." + t.name);
        }
        else if constexpr (std::is_same_v<T, type_ref::Pointer>) {
            write("*");
            emitTypeRef(*t.inner);
        }
        else if constexpr (std::is_same_v<T, type_ref::Slice>) {
            write("[]");
            emitTypeRef(*t.inner);
        }
        else if constexpr (std::is_same_v<T, type_ref::Array>) {
            write("[");
            emitExpr(*t.size);
            write("]");
            emitTypeRef(*t.inner);
        }
        else if constexpr (std::is_same_v<T, type_ref::MapType>) {
            write("map[");
            emitTypeRef(*t.key);
            write("]");
            emitTypeRef(*t.value);
        }
        else if constexpr (std::is_same_v<T, type_ref::Channel>) {
            switch (t.dir) {
                case ChanDir::Both:
                    write("chan ");
                    break;
                case ChanDir::Send:
                    write("chan<- ");
                    break;
                case ChanDir::Recv:
                    write("<-chan ");
                    break;
            }
            emitTypeRef(*t.elem);
        }
        else if constexpr (std::is_same_v<T, type_ref::Result>) {
            write("(");
            emitTypeRef(*t.inner);
            write(", error)");
        }
        else if constexpr (std::is_same_v<T, type_ref::Optional>) {
            write("*");
            emitTypeRef(*t.inner);
        }
        else if constexpr (std::is_same_v<T, type_ref::FuncType>) {
            write("func(");
            for (size_t i = 0; i < t.params.size(); ++i) {
                if (i > 0) write(", ");
                emitTypeRef(*t.params[i]);
            }
            write(")");
            if (!t.returns.empty()) {
                if (t.returns.size() == 1) {
                    write(" ");
                    emitTypeRef(*t.returns[0]);
                } else {
                    write(" (");
                    for (size_t i = 0; i < t.returns.size(); ++i) {
                        if (i > 0) write(", ");
                        emitTypeRef(*t.returns[i]);
                    }
                    write(")");
                }
            }
        }
        else if constexpr (std::is_same_v<T, type_ref::Generic>) {
            emitTypeRef(*t.base);
            write("[");
            for (size_t i = 0; i < t.args.size(); ++i) {
                if (i > 0) write(", ");
                emitTypeRef(*t.args[i]);
            }
            write("]");
        }
    }, type.kind);
}

std::string GoCodegen::zeroValueForType(const TypeRef& type) {
    return std::visit([this, &type](const auto& t) -> std::string {
        using T = std::decay_t<decltype(t)>;
        if constexpr (std::is_same_v<T, type_ref::Named>) {
            if (t.name == "string") return "\"\"";
            if (t.name == "bool") return "false";
            if (t.name == "int" || t.name == "uint" ||
                t.name == "s8" || t.name == "s16" || t.name == "s32" || t.name == "s64" ||
                t.name == "u8" || t.name == "u16" || t.name == "u32" || t.name == "u64" ||
                t.name == "f32" || t.name == "f64" ||
                t.name == "byte" || t.name == "rune") return "0";
            if (t.name == "error") return "nil";
            // Struct types: use zero value
            return t.name + "{}";
        }
        if constexpr (std::is_same_v<T, type_ref::Qualified>) {
            return t.pkg + "." + t.name + "{}";
        }
        if constexpr (std::is_same_v<T, type_ref::Pointer> ||
                       std::is_same_v<T, type_ref::Slice> ||
                       std::is_same_v<T, type_ref::MapType> ||
                       std::is_same_v<T, type_ref::Channel> ||
                       std::is_same_v<T, type_ref::Optional> ||
                       std::is_same_v<T, type_ref::FuncType>) {
            return "nil";
        }
        if constexpr (std::is_same_v<T, type_ref::Array>) {
            return "{}"; // simplified
        }
        if constexpr (std::is_same_v<T, type_ref::Result>) {
            // Shouldn't normally happen directly
            return zeroValueForType(*t.inner);
        }
        if constexpr (std::is_same_v<T, type_ref::Generic>) {
            return typeRefToString(type) + "{}";
        }
        return "nil";
    }, type.kind);
}

bool GoCodegen::containsTry(const Expr& expr) {
    return std::holds_alternative<expr::Try>(expr.kind);
}

std::string GoCodegen::typeRefToString(const TypeRef& type) {
    std::ostringstream tmp;
    std::swap(tmp, out_);
    emitTypeRef(type);
    std::string result = out_.str();
    std::swap(tmp, out_);
    return result;
}

} // namespace zo
