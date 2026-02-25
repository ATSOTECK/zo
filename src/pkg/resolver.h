#pragma once

#include "pkg/build_config.h"
#include "pkg/git_ops.h"
#include "pkg/lock_file.h"
#include "pkg/semver.h"

#include <optional>
#include <string>
#include <vector>

namespace zo {

struct ResolvedDep {
    std::string name;
    std::string git;
    std::string version;     // resolved version string, e.g. "v1.2.3"
    std::string commitHash;
};

class Resolver {
public:
    explicit Resolver(GitOps& gitOps);

    // Resolve all dependencies against remote tags and lock file.
    // Returns resolved deps, or empty vector on failure.
    std::vector<ResolvedDep> resolve(const std::vector<Dependency>& deps,
                                     const std::optional<LockFile>& lock);

    // Resolve a single dependency. Returns nullopt on failure.
    std::optional<ResolvedDep> resolveOne(const Dependency& dep,
                                          const LockFile* lock);

private:
    GitOps& gitOps_;
};

} // namespace zo
