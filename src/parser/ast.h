#pragma once

#include "common/source_location.h"
#include "lexer/token.h"

#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace zo {

// Forward declarations
struct Expr;
struct Stmt;

using ExprPtr = std::unique_ptr<Expr>;
using StmtPtr = std::unique_ptr<Stmt>;

// ─── Type references ────────────────────────────────────────────────
// Represents a type as written in source code (not a resolved semantic type).

struct TypeRef;
using TypeRefPtr = std::unique_ptr<TypeRef>;

enum class ChanDir { Both, Send, Recv };

namespace type_ref {
    struct Named { std::string name; };                              // int, string, Point
    struct Qualified { std::string pkg; std::string name; };         // pkg::Type
    struct Pointer { TypeRefPtr inner; };                            // *T
    struct Slice { TypeRefPtr inner; };                              // []T
    struct Array { ExprPtr size; TypeRefPtr inner; };                // [N]T
    struct MapType { TypeRefPtr key; TypeRefPtr value; };            // map[K]V
    struct Channel { ChanDir dir; TypeRefPtr elem; };                // chan T, <-chan T, chan<- T
    struct Result { TypeRefPtr inner; };                             // T!
    struct Optional { TypeRefPtr inner; };                           // T?
    struct FuncType {
        std::vector<TypeRefPtr> params;
        std::vector<TypeRefPtr> returns;
    };
}

using TypeRefKind = std::variant<
    type_ref::Named,
    type_ref::Qualified,
    type_ref::Pointer,
    type_ref::Slice,
    type_ref::Array,
    type_ref::MapType,
    type_ref::Channel,
    type_ref::Result,
    type_ref::Optional,
    type_ref::FuncType
>;

struct TypeRef {
    TypeRefKind kind;
    SourceLocation loc;
};

// ─── Fields & Params (needed by expressions and declarations) ───────

struct Field {
    std::string name;
    TypeRefPtr type;
    SourceLocation loc;
};

struct Param {
    std::string name;
    TypeRefPtr type;
    SourceLocation loc;
};

// ─── Expressions ────────────────────────────────────────────────────

namespace expr {
    struct Ident       { std::string name; };
    struct StringLit   { std::string value; };
    struct IntLit      { std::string value; };  // Keep as string for faithful codegen
    struct FloatLit    { std::string value; };
    struct BoolLit     { bool value; };
    struct NilLit      {};
    struct This        {};
    struct ScopeAccess { std::string scope; std::string member; };  // pkg::Func
    struct Selector    { ExprPtr object; std::string member; };     // obj.field
    struct Call        { ExprPtr callee; std::vector<ExprPtr> args; };
    struct Index       { ExprPtr object; ExprPtr index; };          // a[i]
    struct Unary       { TokenKind op; ExprPtr operand; };
    struct Binary      { ExprPtr left; TokenKind op; ExprPtr right; };
    struct Assign      { ExprPtr target; TokenKind op; ExprPtr value; };  // =, +=, etc.
    struct ShortDecl   { std::vector<std::string> names; ExprPtr value; };  // x := expr
    struct CompositeLit {
        TypeRefPtr type;
        std::vector<ExprPtr> elements;  // can contain KeyValue exprs
    };
    struct KeyValue    { ExprPtr key; ExprPtr value; };  // key: value in composite literals
    struct Closure {
        std::vector<Param> params;
        std::vector<TypeRefPtr> returns;
        std::vector<StmtPtr> body;
    };
    struct SliceExpr   { ExprPtr object; ExprPtr low; ExprPtr high; };  // a[lo:hi]
    struct TypeAssert  { ExprPtr object; TypeRefPtr type; };  // x.(Type)
    struct EnumVariant {
        std::optional<std::string> type_name;  // "Color" or empty for shorthand
        std::string variant;                    // "Red"
    };
    struct UnionVariant {
        std::optional<std::string> type_name;
        std::string variant;
        std::vector<std::string> bindings;  // destructured variable names
    };
}

using ExprKind = std::variant<
    expr::Ident,
    expr::StringLit,
    expr::IntLit,
    expr::FloatLit,
    expr::BoolLit,
    expr::NilLit,
    expr::This,
    expr::ScopeAccess,
    expr::Selector,
    expr::Call,
    expr::Index,
    expr::Unary,
    expr::Binary,
    expr::Assign,
    expr::ShortDecl,
    expr::CompositeLit,
    expr::KeyValue,
    expr::Closure,
    expr::SliceExpr,
    expr::TypeAssert,
    expr::EnumVariant,
    expr::UnionVariant
>;

struct Expr {
    ExprKind kind;
    SourceLocation loc;
};

// ─── Statements ─────────────────────────────────────────────────────

struct MatchArm {
    std::vector<ExprPtr> patterns;  // enum variants, literals, or Ident("_")
    std::vector<StmtPtr> body;
    SourceLocation loc;
};

struct SelectCase {
    std::optional<StmtPtr> comm;    // nullopt = default case
    std::vector<StmtPtr> body;
};

namespace stmt {
    struct ExprStmt   { ExprPtr expr; };
    struct Return     { std::vector<ExprPtr> values; };
    struct VarDecl    {
        std::string name;
        std::optional<TypeRefPtr> type;
        std::optional<ExprPtr> init;
    };
    struct ConstDecl  {
        std::string name;
        std::optional<TypeRefPtr> type;
        ExprPtr init;
    };
    struct Block      { std::vector<StmtPtr> stmts; };
    struct If         {
        ExprPtr cond;
        std::unique_ptr<stmt::Block> then_block;
        std::optional<StmtPtr> else_block;  // another If or Block
    };
    struct For        {
        std::optional<StmtPtr> init;
        std::optional<ExprPtr> cond;
        std::optional<StmtPtr> post;
        std::unique_ptr<stmt::Block> body;
    };
    struct ForRange   {
        std::string key;
        std::optional<std::string> value;
        bool define;  // true for :=, false for = (assign)
        ExprPtr iterable;
        std::unique_ptr<stmt::Block> body;
    };
    struct Select     {
        std::vector<SelectCase> cases;
    };
    struct Send       { ExprPtr channel; ExprPtr value; };   // ch <- value
    struct GoStmt     { ExprPtr call; };
    struct DeferStmt  { ExprPtr call; };
    struct IncDec     { ExprPtr operand; TokenKind op; };  // ++ or --
    struct Match      {
        ExprPtr value;
        std::vector<MatchArm> arms;
    };
    struct Break      {};
    struct Continue   {};
    struct Fallthrough{};
}

using StmtKind = std::variant<
    stmt::ExprStmt,
    stmt::Return,
    stmt::VarDecl,
    stmt::ConstDecl,
    stmt::Block,
    stmt::If,
    stmt::For,
    stmt::ForRange,
    stmt::Select,
    stmt::Match,
    stmt::Send,
    stmt::GoStmt,
    stmt::DeferStmt,
    stmt::IncDec,
    stmt::Break,
    stmt::Continue,
    stmt::Fallthrough
>;

struct Stmt {
    StmtKind kind;
    SourceLocation loc;
};

// ─── Top-level declarations ─────────────────────────────────────────

struct EnumVariantDef {
    std::string name;
    std::optional<ExprPtr> value;  // explicit value (e.g., Active = 1)
    SourceLocation loc;
};

struct UnionVariantDef {
    std::string name;
    std::vector<Field> fields;  // associated data fields
    SourceLocation loc;
};

namespace decl {
    struct Package {
        std::string name;
    };

    struct Import {
        std::string name;
        std::optional<std::string> alias;
    };

    struct Func {
        std::string name;
        std::vector<Param> params;
        std::vector<TypeRefPtr> returns;  // empty = void
        std::vector<StmtPtr> body;
    };

    struct Struct {
        std::string name;
        std::vector<Field> fields;
    };

    struct Interface {
        std::string name;
        std::vector<Func> methods;
    };

    struct ImplBlock {
        std::string target;
        std::optional<std::string> interface_name;  // for "impl T implements I"
        std::vector<Func> methods;
    };

    struct TypeAlias {
        std::string name;
        TypeRefPtr type;
    };

    struct Enum {
        std::string name;
        std::optional<TypeRefPtr> underlying_type;  // for enum Shape: f32
        std::vector<EnumVariantDef> variants;
    };

    struct Union {
        std::string name;
        std::vector<UnionVariantDef> variants;
    };
}

using DeclKind = std::variant<
    decl::Package,
    decl::Import,
    decl::Func,
    decl::Struct,
    decl::Interface,
    decl::ImplBlock,
    decl::TypeAlias,
    decl::Enum,
    decl::Union
>;

struct Decl {
    DeclKind kind;
    SourceLocation loc;
};

// ─── File (translation unit) ────────────────────────────────────────

struct File {
    std::string filename;
    std::vector<Decl> decls;
};

// ─── Helpers ────────────────────────────────────────────────────────

template<typename T, typename... Args>
ExprPtr makeExpr(SourceLocation loc, Args&&... args) {
    return std::make_unique<Expr>(Expr{T{std::forward<Args>(args)...}, loc});
}

template<typename T, typename... Args>
StmtPtr makeStmt(SourceLocation loc, Args&&... args) {
    return std::make_unique<Stmt>(Stmt{T{std::forward<Args>(args)...}, loc});
}

template<typename T, typename... Args>
TypeRefPtr makeTypeRef(SourceLocation loc, Args&&... args) {
    return std::make_unique<TypeRef>(TypeRef{T{std::forward<Args>(args)...}, loc});
}

} // namespace zo
