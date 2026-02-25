#pragma once

#include <optional>
#include <string>
#include <vector>

namespace zo {

struct LockedDependency {
    std::string name;
    std::string git;
    std::string version;   // resolved version, e.g. "v1.2.3"
    std::string commit;    // commit hash
};

struct LockFile {
    std::vector<LockedDependency> dependencies;

    static std::optional<LockFile> load(const std::string& dir);
    bool save(const std::string& dir) const;

    // Find a locked dep by name. Returns nullptr if not found.
    const LockedDependency* find(const std::string& name) const;

    void setDependency(const LockedDependency& dep);
};

} // namespace zo
