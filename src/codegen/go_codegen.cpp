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

    for (const auto& decl : file.decls) {
        emitDecl(decl);
    }

    return out_.str();
}

void GoCodegen::emitDecl(const Decl& decl) {
    std::visit([this](const auto& d) {
        using T = std::decay_t<decltype(d)>;

        if constexpr (std::is_same_v<T, decl::Package>) {
            writeln("package " + d.name);
            write("\n");
        }
        else if constexpr (std::is_same_v<T, decl::Import>) {
            if (d.alias.has_value()) {
                writeln("import " + d.alias.value() + " \"" + d.name + "\"");
            } else {
                writeln("import \"" + d.name + "\"");
            }
            write("\n");
        }
        else if constexpr (std::is_same_v<T, decl::Func>) {
            emitFunc(d);
            write("\n");
        }
        else if constexpr (std::is_same_v<T, decl::Struct>) {
            writeln("type " + d.name + " struct {");
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
            for (const auto& method : d.methods) {
                emitFunc(method, d.target);
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
    }, decl.kind);
}

void GoCodegen::emitFunc(const decl::Func& fn, const std::string& receiver) {
    writeIndent();
    write("func ");

    if (!receiver.empty()) {
        write("(this *" + receiver + ") ");
    }

    write(fn.name + "(");
    for (size_t i = 0; i < fn.params.size(); ++i) {
        if (i > 0) write(", ");
        write(fn.params[i].name + " ");
        emitTypeRef(*fn.params[i].type);
    }
    write(")");

    emitReturnTypes(fn.returns);

    if (fn.body.empty() && fn.returns.empty()) {
        write("\n");
        return;
    }

    write(" {\n");
    indent();
    for (const auto& stmt : fn.body) {
        emitStmt(*stmt);
    }
    dedent();
    writeln("}");
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
            writeIndent();
            emitExpr(*s.expr);
            write("\n");
        }
        else if constexpr (std::is_same_v<T, stmt::Return>) {
            writeIndent();
            write("return");
            for (size_t i = 0; i < s.values.size(); ++i) {
                write(i == 0 ? " " : ", ");
                emitExpr(*s.values[i]);
            }
            write("\n");
        }
        else if constexpr (std::is_same_v<T, stmt::VarDecl>) {
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
        else if constexpr (std::is_same_v<T, stmt::Switch>) {
            writeIndent();
            write("switch ");
            if (s.tag.has_value()) {
                emitExpr(**s.tag);
                write(" ");
            }
            write("{\n");
            for (const auto& c : s.cases) {
                writeIndent();
                if (c.values.empty()) {
                    write("default:\n");
                } else {
                    write("case ");
                    for (size_t i = 0; i < c.values.size(); ++i) {
                        if (i > 0) write(", ");
                        emitExpr(*c.values[i]);
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
    }, expr.kind);
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
    }, type.kind);
}

} // namespace zo
