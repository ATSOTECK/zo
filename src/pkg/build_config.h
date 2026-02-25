#pragma once

#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

namespace zo {

struct Dependency {
    std::string name;       // short name, e.g. "rage"
    std::string git;        // e.g. "github.com/ATSOTECK/rage"
    std::string version;    // e.g. "^1.2.0" or "v5.0.0"
    std::string path;       // sub-path within module, e.g. "pkg/rage" (may be empty)
};

struct PackageInfo {
    std::string name;
    std::string version;
    std::string zo;
};

struct GoConfig {
    std::string version = "1.21";
};

struct BuildConfig {
    PackageInfo package;
    std::vector<Dependency> dependencies;
    GoConfig go;

    static std::optional<BuildConfig> load(const std::string& dir);
    bool save(const std::string& dir) const;

    // Build map of {short_name -> full_go_import_path}
    std::unordered_map<std::string, std::string> buildImportMap() const;

    void addDependency(const Dependency& dep);
};

} // namespace zo
