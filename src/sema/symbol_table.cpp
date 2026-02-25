#include "sema/symbol_table.h"

namespace zo {

bool Scope::define(const std::string& name, Symbol symbol) {
    auto [it, inserted] = symbols_.emplace(name, std::move(symbol));
    return inserted;
}

Symbol* Scope::lookup(const std::string& name) {
    auto it = symbols_.find(name);
    if (it != symbols_.end()) return &it->second;
    if (parent_) return parent_->lookup(name);
    return nullptr;
}

Symbol* Scope::lookupLocal(const std::string& name) {
    auto it = symbols_.find(name);
    if (it != symbols_.end()) return &it->second;
    return nullptr;
}

SymbolTable::SymbolTable() {
    // Create the global scope
    scopes_.push_back(std::make_unique<Scope>(nullptr));
    current_ = scopes_.back().get();
}

void SymbolTable::enterScope() {
    scopes_.push_back(std::make_unique<Scope>(current_));
    current_ = scopes_.back().get();
}

void SymbolTable::exitScope() {
    if (current_->parent()) {
        current_ = current_->parent();
    }
}

bool SymbolTable::define(const std::string& name, Symbol symbol) {
    return current_->define(name, std::move(symbol));
}

Symbol* SymbolTable::lookup(const std::string& name) {
    return current_->lookup(name);
}

Symbol* SymbolTable::lookupLocal(const std::string& name) {
    return current_->lookupLocal(name);
}

} // namespace zo
