#pragma once

#include "common/source_location.h"

#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

namespace zo {

enum class SymbolKind {
    Variable,
    Function,
    Type,
    Package,
    Constant,
};

struct Symbol {
    std::string name;
    SymbolKind kind;
    SourceLocation loc;
    std::string type_name;  // simplified type representation for Phase 1
};

class Scope {
public:
    explicit Scope(Scope* parent = nullptr) : parent_(parent) {}

    bool define(const std::string& name, Symbol symbol);
    Symbol* lookup(const std::string& name);
    Symbol* lookupLocal(const std::string& name);

    Scope* parent() const { return parent_; }

private:
    Scope* parent_;
    std::unordered_map<std::string, Symbol> symbols_;
};

class SymbolTable {
public:
    SymbolTable();

    void enterScope();
    void exitScope();

    bool define(const std::string& name, Symbol symbol);
    Symbol* lookup(const std::string& name);
    Symbol* lookupLocal(const std::string& name);

    Scope* currentScope() const { return current_; }

private:
    std::vector<std::unique_ptr<Scope>> scopes_;
    Scope* current_ = nullptr;
};

} // namespace zo
