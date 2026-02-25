#include "sema/type_checker.h"

#include <unordered_set>
#include <variant>

namespace zo {

TypeChecker::TypeChecker(DiagnosticEngine& diag) : diag_(diag) {
    registerBuiltins();
}

void TypeChecker::registerBuiltins() {
    // Register built-in types
    symbols_.define("int",    Symbol{"int",    SymbolKind::Type, {}, "int"});
    symbols_.define("uint",   Symbol{"uint",   SymbolKind::Type, {}, "uint"});
    symbols_.define("s8",     Symbol{"s8",     SymbolKind::Type, {}, "s8"});
    symbols_.define("s16",    Symbol{"s16",    SymbolKind::Type, {}, "s16"});
    symbols_.define("s32",    Symbol{"s32",    SymbolKind::Type, {}, "s32"});
    symbols_.define("s64",    Symbol{"s64",    SymbolKind::Type, {}, "s64"});
    symbols_.define("u8",     Symbol{"u8",     SymbolKind::Type, {}, "u8"});
    symbols_.define("u16",    Symbol{"u16",    SymbolKind::Type, {}, "u16"});
    symbols_.define("u32",    Symbol{"u32",    SymbolKind::Type, {}, "u32"});
    symbols_.define("u64",    Symbol{"u64",    SymbolKind::Type, {}, "u64"});
    symbols_.define("f32",    Symbol{"f32",    SymbolKind::Type, {}, "f32"});
    symbols_.define("f64",    Symbol{"f64",    SymbolKind::Type, {}, "f64"});
    symbols_.define("string", Symbol{"string", SymbolKind::Type, {}, "string"});
    symbols_.define("bool",   Symbol{"bool",   SymbolKind::Type, {}, "bool"});
    symbols_.define("byte",   Symbol{"byte",   SymbolKind::Type, {}, "byte"});
    symbols_.define("error",  Symbol{"error",  SymbolKind::Type, {}, "error"});
    symbols_.define("rune",   Symbol{"rune",   SymbolKind::Type, {}, "rune"});

    // Register built-in functions
    symbols_.define("make",    Symbol{"make",    SymbolKind::Function, {}, "builtin"});
    symbols_.define("append",  Symbol{"append",  SymbolKind::Function, {}, "builtin"});
    symbols_.define("len",     Symbol{"len",     SymbolKind::Function, {}, "builtin"});
    symbols_.define("cap",     Symbol{"cap",     SymbolKind::Function, {}, "builtin"});
    symbols_.define("close",   Symbol{"close",   SymbolKind::Function, {}, "builtin"});
    symbols_.define("delete",  Symbol{"delete",  SymbolKind::Function, {}, "builtin"});
    symbols_.define("panic",   Symbol{"panic",   SymbolKind::Function, {}, "builtin"});
    symbols_.define("recover", Symbol{"recover", SymbolKind::Function, {}, "builtin"});
    symbols_.define("print",   Symbol{"print",   SymbolKind::Function, {}, "builtin"});
    symbols_.define("println", Symbol{"println", SymbolKind::Function, {}, "builtin"});
    symbols_.define("copy",    Symbol{"copy",    SymbolKind::Function, {}, "builtin"});
    symbols_.define("new",     Symbol{"new",     SymbolKind::Function, {}, "builtin"});

    // Constants
    symbols_.define("true",  Symbol{"true",  SymbolKind::Constant, {}, "bool"});
    symbols_.define("false", Symbol{"false", SymbolKind::Constant, {}, "bool"});
    symbols_.define("nil",   Symbol{"nil",   SymbolKind::Constant, {}, "nil"});
    symbols_.define("iota",  Symbol{"iota",  SymbolKind::Constant, {}, "int"});
}

void TypeChecker::check(const File& file) {
    // First pass: register all top-level declarations
    for (const auto& decl : file.decls) {
        registerDecl(decl);
    }

    // Second pass: check all declarations
    for (const auto& decl : file.decls) {
        checkDecl(decl);
    }
}

void TypeChecker::registerDecl(const Decl& decl) {
    std::visit([this, &decl](const auto& d) {
        using T = std::decay_t<decltype(d)>;

        if constexpr (std::is_same_v<T, decl::Package>) {
            currentPackage_ = d.name;
        }
        else if constexpr (std::is_same_v<T, decl::Import>) {
            auto name = d.alias.value_or(d.name);
            symbols_.define(name, Symbol{name, SymbolKind::Package, decl.loc, d.name});
        }
        else if constexpr (std::is_same_v<T, decl::Func>) {
            symbols_.define(d.name, Symbol{d.name, SymbolKind::Function, decl.loc, "func"});
        }
        else if constexpr (std::is_same_v<T, decl::Struct>) {
            symbols_.define(d.name, Symbol{d.name, SymbolKind::Type, decl.loc, d.name});
        }
        else if constexpr (std::is_same_v<T, decl::Interface>) {
            symbols_.define(d.name, Symbol{d.name, SymbolKind::Type, decl.loc, d.name});
        }
        else if constexpr (std::is_same_v<T, decl::ImplBlock>) {
            // impl blocks don't introduce new names
        }
        else if constexpr (std::is_same_v<T, decl::TypeAlias>) {
            symbols_.define(d.name, Symbol{d.name, SymbolKind::Type, decl.loc, d.name});
        }
        else if constexpr (std::is_same_v<T, decl::Enum>) {
            symbols_.define(d.name, Symbol{d.name, SymbolKind::Type, decl.loc, d.name});
            EnumMeta meta;
            meta.name = d.name;
            for (const auto& v : d.variants) {
                meta.variants.push_back(v.name);
                // Register each variant as a constant
                symbols_.define(d.name + v.name,
                    Symbol{d.name + v.name, SymbolKind::Constant, v.loc, d.name});
            }
            enumTypes_[d.name] = std::move(meta);
        }
        else if constexpr (std::is_same_v<T, decl::Union>) {
            symbols_.define(d.name, Symbol{d.name, SymbolKind::Type, decl.loc, d.name});
            EnumMeta meta;
            meta.name = d.name;
            for (const auto& v : d.variants) {
                meta.variants.push_back(v.name);
                symbols_.define(d.name + v.name,
                    Symbol{d.name + v.name, SymbolKind::Type, v.loc, d.name});
            }
            unionTypes_[d.name] = std::move(meta);
        }
        else if constexpr (std::is_same_v<T, decl::Constraint>) {
            symbols_.define(d.name, Symbol{d.name, SymbolKind::Type, decl.loc, d.name});
        }
        else if constexpr (std::is_same_v<T, decl::ErrorEnum>) {
            symbols_.define(d.name, Symbol{d.name, SymbolKind::Type, decl.loc, d.name});
            EnumMeta meta;
            meta.name = d.name;
            for (const auto& v : d.variants) {
                meta.variants.push_back(v.name);
                symbols_.define(d.name + v.name,
                    Symbol{d.name + v.name, SymbolKind::Constant, v.loc, d.name});
            }
            enumTypes_[d.name] = std::move(meta);
        }
    }, decl.kind);
}

void TypeChecker::checkDecl(const Decl& decl) {
    std::visit([this](const auto& d) {
        using T = std::decay_t<decltype(d)>;

        if constexpr (std::is_same_v<T, decl::Func>) {
            checkFunc(d);
        }
        else if constexpr (std::is_same_v<T, decl::Enum>) {
            // Check for duplicate variants
            std::unordered_set<std::string> seen;
            for (const auto& v : d.variants) {
                if (!seen.insert(v.name).second) {
                    diag_.error(v.loc, "duplicate enum variant '" + v.name + "'");
                }
                if (v.value.has_value()) {
                    checkExpr(**v.value);
                }
            }
        }
        else if constexpr (std::is_same_v<T, decl::Struct>) {
            // Register type params for generic structs
            if (!d.type_params.empty()) {
                symbols_.enterScope();
                for (const auto& tp : d.type_params) {
                    symbols_.define(tp.name, Symbol{tp.name, SymbolKind::Type, tp.loc, "type_param"});
                }
                symbols_.exitScope();
            }
        }
        else if constexpr (std::is_same_v<T, decl::Union>) {
            // Check for duplicate variants
            std::unordered_set<std::string> seen;
            for (const auto& v : d.variants) {
                if (!seen.insert(v.name).second) {
                    diag_.error(v.loc, "duplicate union variant '" + v.name + "'");
                }
            }
        }
        else if constexpr (std::is_same_v<T, decl::ImplBlock>) {
            // Verify the target type exists
            if (!symbols_.lookup(d.target)) {
                diag_.error(SourceLocation{}, "impl target type '" + d.target + "' is not defined");
            }
            // Check interface exists if implementing one
            if (d.interface_name.has_value()) {
                if (!symbols_.lookup(d.interface_name.value())) {
                    diag_.error(SourceLocation{},
                        "interface '" + d.interface_name.value() + "' is not defined");
                }
            }
            // Register impl-level type params before checking methods
            symbols_.enterScope();
            for (const auto& tp : d.type_params) {
                symbols_.define(tp.name, Symbol{tp.name, SymbolKind::Type, tp.loc, "type_param"});
            }
            // Check each method
            for (const auto& method : d.methods) {
                checkFunc(method);
            }
            symbols_.exitScope();
        }
        else if constexpr (std::is_same_v<T, decl::Constraint>) {
            // Already registered in registerDecl
        }
        else if constexpr (std::is_same_v<T, decl::ErrorEnum>) {
            // Check for duplicate variants
            std::unordered_set<std::string> seen;
            for (const auto& v : d.variants) {
                if (!seen.insert(v.name).second) {
                    diag_.error(v.loc, "duplicate error variant '" + v.name + "'");
                }
            }
        }
    }, decl.kind);
}

void TypeChecker::checkFunc(const decl::Func& fn) {
    symbols_.enterScope();

    // Register type parameters as types in scope
    for (const auto& tp : fn.type_params) {
        symbols_.define(tp.name, Symbol{tp.name, SymbolKind::Type, tp.loc, "type_param"});
        if (tp.constraint.has_value()) {
            if (!symbols_.lookup(tp.constraint.value())) {
                diag_.error(tp.loc, "undeclared constraint '" + tp.constraint.value() + "'");
            }
        }
    }

    // Detect T! return type
    bool prevInResult = inResultFunc_;
    inResultFunc_ = false;
    for (const auto& ret : fn.returns) {
        if (std::holds_alternative<type_ref::Result>(ret->kind)) {
            inResultFunc_ = true;
            break;
        }
    }

    // Register parameters
    for (const auto& param : fn.params) {
        symbols_.define(param.name, Symbol{param.name, SymbolKind::Variable, param.loc, "param"});
    }

    // Check body
    checkBlock(fn.body);

    inResultFunc_ = prevInResult;
    symbols_.exitScope();
}

void TypeChecker::checkBlock(const std::vector<StmtPtr>& stmts) {
    for (const auto& stmt : stmts) {
        checkStmt(*stmt);
    }
}

void TypeChecker::checkStmt(const Stmt& stmt) {
    std::visit([this](const auto& s) {
        using T = std::decay_t<decltype(s)>;

        if constexpr (std::is_same_v<T, stmt::ExprStmt>) {
            checkExpr(*s.expr);
        }
        else if constexpr (std::is_same_v<T, stmt::Return>) {
            for (const auto& val : s.values) {
                checkExpr(*val);
            }
        }
        else if constexpr (std::is_same_v<T, stmt::VarDecl>) {
            if (s.init.has_value()) {
                checkExpr(**s.init);
            }
            symbols_.define(s.name, Symbol{s.name, SymbolKind::Variable, {}, "var"});
        }
        else if constexpr (std::is_same_v<T, stmt::ConstDecl>) {
            checkExpr(*s.init);
            symbols_.define(s.name, Symbol{s.name, SymbolKind::Constant, {}, "const"});
        }
        else if constexpr (std::is_same_v<T, stmt::Block>) {
            symbols_.enterScope();
            checkBlock(s.stmts);
            symbols_.exitScope();
        }
        else if constexpr (std::is_same_v<T, stmt::If>) {
            checkExpr(*s.cond);
            symbols_.enterScope();
            checkBlock(s.then_block->stmts);
            symbols_.exitScope();
            if (s.else_block.has_value()) {
                checkStmt(**s.else_block);
            }
        }
        else if constexpr (std::is_same_v<T, stmt::For>) {
            symbols_.enterScope();
            if (s.init.has_value()) checkStmt(**s.init);
            if (s.cond.has_value()) checkExpr(**s.cond);
            if (s.post.has_value()) checkStmt(**s.post);
            checkBlock(s.body->stmts);
            symbols_.exitScope();
        }
        else if constexpr (std::is_same_v<T, stmt::ForRange>) {
            symbols_.enterScope();
            checkExpr(*s.iterable);
            symbols_.define(s.key, Symbol{s.key, SymbolKind::Variable, {}, "range_key"});
            if (s.value.has_value()) {
                symbols_.define(s.value.value(), Symbol{s.value.value(), SymbolKind::Variable, {}, "range_val"});
            }
            checkBlock(s.body->stmts);
            symbols_.exitScope();
        }
        else if constexpr (std::is_same_v<T, stmt::Select>) {
            for (const auto& c : s.cases) {
                symbols_.enterScope();
                if (c.comm.has_value()) checkStmt(**c.comm);
                checkBlock(c.body);
                symbols_.exitScope();
            }
        }
        else if constexpr (std::is_same_v<T, stmt::Match>) {
            checkExpr(*s.value);
            for (const auto& arm : s.arms) {
                symbols_.enterScope();
                // Register destructured bindings from union variant patterns
                for (const auto& pat : arm.patterns) {
                    if (auto* uv = std::get_if<expr::UnionVariant>(&pat->kind)) {
                        for (const auto& binding : uv->bindings) {
                            symbols_.define(binding,
                                Symbol{binding, SymbolKind::Variable, pat->loc, "match_binding"});
                        }
                    }
                }
                checkBlock(arm.body);
                symbols_.exitScope();
            }
        }
        else if constexpr (std::is_same_v<T, stmt::Send>) {
            checkExpr(*s.channel);
            checkExpr(*s.value);
        }
        else if constexpr (std::is_same_v<T, stmt::GoStmt>) {
            checkExpr(*s.call);
        }
        else if constexpr (std::is_same_v<T, stmt::DeferStmt>) {
            checkExpr(*s.call);
        }
        else if constexpr (std::is_same_v<T, stmt::IncDec>) {
            checkExpr(*s.operand);
        }
        // Break, Continue, Fallthrough — nothing to check
    }, stmt.kind);
}

void TypeChecker::checkExpr(const Expr& expr) {
    std::visit([this, &expr](const auto& e) {
        using T = std::decay_t<decltype(e)>;

        if constexpr (std::is_same_v<T, expr::Ident>) {
            if (!symbols_.lookup(e.name)) {
                diag_.error(expr.loc, "undeclared identifier '" + e.name + "'");
            }
        }
        else if constexpr (std::is_same_v<T, expr::ScopeAccess>) {
            // Check that the package is imported
            if (!symbols_.lookup(e.scope)) {
                diag_.error(expr.loc, "undeclared package '" + e.scope + "'");
            }
        }
        else if constexpr (std::is_same_v<T, expr::Selector>) {
            checkExpr(*e.object);
        }
        else if constexpr (std::is_same_v<T, expr::Call>) {
            checkExpr(*e.callee);
            for (const auto& arg : e.args) {
                checkExpr(*arg);
            }
        }
        else if constexpr (std::is_same_v<T, expr::Index>) {
            checkExpr(*e.object);
            checkExpr(*e.index);
        }
        else if constexpr (std::is_same_v<T, expr::Unary>) {
            checkExpr(*e.operand);
        }
        else if constexpr (std::is_same_v<T, expr::Binary>) {
            checkExpr(*e.left);
            checkExpr(*e.right);
        }
        else if constexpr (std::is_same_v<T, expr::Assign>) {
            checkExpr(*e.target);
            checkExpr(*e.value);
        }
        else if constexpr (std::is_same_v<T, expr::ShortDecl>) {
            checkExpr(*e.value);
            for (const auto& name : e.names) {
                if (name != "_") {
                    symbols_.define(name, Symbol{name, SymbolKind::Variable, expr.loc, "short_decl"});
                }
            }
        }
        else if constexpr (std::is_same_v<T, expr::CompositeLit>) {
            for (const auto& elem : e.elements) {
                checkExpr(*elem);
            }
        }
        else if constexpr (std::is_same_v<T, expr::KeyValue>) {
            // Don't check key as identifier (it could be a field name)
            checkExpr(*e.value);
        }
        else if constexpr (std::is_same_v<T, expr::Closure>) {
            symbols_.enterScope();
            for (const auto& param : e.params) {
                symbols_.define(param.name, Symbol{param.name, SymbolKind::Variable, param.loc, "param"});
            }
            checkBlock(e.body);
            symbols_.exitScope();
        }
        else if constexpr (std::is_same_v<T, expr::SliceExpr>) {
            checkExpr(*e.object);
            checkExpr(*e.low);
            checkExpr(*e.high);
        }
        else if constexpr (std::is_same_v<T, expr::TypeAssert>) {
            checkExpr(*e.object);
        }
        else if constexpr (std::is_same_v<T, expr::EnumVariant>) {
            // Validate the variant exists in the named enum/union
            if (e.type_name.has_value()) {
                auto it = enumTypes_.find(*e.type_name);
                if (it != enumTypes_.end()) {
                    bool found = false;
                    for (const auto& v : it->second.variants) {
                        if (v == e.variant) { found = true; break; }
                    }
                    if (!found) {
                        diag_.error(expr.loc, "'" + e.variant + "' is not a variant of '" + *e.type_name + "'");
                    }
                }
            }
        }
        else if constexpr (std::is_same_v<T, expr::UnionVariant>) {
            // Union variant patterns — validated in match context
        }
        else if constexpr (std::is_same_v<T, expr::Try>) {
            checkExpr(*e.operand);
            if (!inResultFunc_) {
                diag_.warning(expr.loc, "'try' used outside of a function returning T!");
            }
        }
        else if constexpr (std::is_same_v<T, expr::Else>) {
            checkExpr(*e.value);
            checkExpr(*e.fallback);
        }
        else if constexpr (std::is_same_v<T, expr::Catch>) {
            checkExpr(*e.operand);
            symbols_.enterScope();
            symbols_.define(e.errVar, Symbol{e.errVar, SymbolKind::Variable, expr.loc, "catch_err"});
            checkBlock(e.body);
            symbols_.exitScope();
        }
        // Literals (StringLit, IntLit, FloatLit, BoolLit, NilLit, This) — nothing to check
    }, expr.kind);
}

} // namespace zo
