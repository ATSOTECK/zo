#include "pkg/resolver.h"

#include <algorithm>
#include <iostream>

namespace zo {

Resolver::Resolver(GitOps& gitOps) : gitOps_(gitOps) {}

std::vector<ResolvedDep> Resolver::resolve(const std::vector<Dependency>& deps,
                                            const std::optional<LockFile>& lock) {
    std::vector<ResolvedDep> resolved;
    const LockFile* lockPtr = lock.has_value() ? &lock.value() : nullptr;

    for (const auto& dep : deps) {
        auto result = resolveOne(dep, lockPtr);
        if (!result) {
            std::cerr << "error: failed to resolve dependency '" << dep.name << "'\n";
            return {};
        }
        resolved.push_back(std::move(*result));
    }

    return resolved;
}

std::optional<ResolvedDep> Resolver::resolveOne(const Dependency& dep,
                                                 const LockFile* lock) {
    // Parse the version constraint
    auto constraint = VersionConstraint::parse(dep.version);
    if (!constraint) {
        std::cerr << "error: invalid version constraint '" << dep.version
                  << "' for dependency '" << dep.name << "'\n";
        return std::nullopt;
    }

    // Check lock file first
    if (lock) {
        if (auto locked = lock->find(dep.name)) {
            auto lockedVer = SemVer::parse(locked->version);
            if (lockedVer && constraint->satisfiedBy(*lockedVer)) {
                return ResolvedDep{
                    dep.name,
                    dep.git,
                    locked->version,
                    locked->commit
                };
            }
        }
    }

    // Fetch remote tags
    auto remoteTags = gitOps_.listRemoteTags(dep.git);
    if (remoteTags.empty()) {
        std::cerr << "error: no tags found for '" << dep.git << "'\n";
        return std::nullopt;
    }

    // Parse tags as semver
    std::vector<std::pair<SemVer, std::string>> candidates;  // version, commit
    for (const auto& tag : remoteTags) {
        auto ver = SemVer::parse(tag.name);
        if (ver) {
            candidates.push_back({*ver, tag.commitHash});
        }
    }

    if (candidates.empty()) {
        std::cerr << "error: no valid semver tags found for '" << dep.git << "'\n";
        return std::nullopt;
    }

    // Sort ascending
    std::sort(candidates.begin(), candidates.end(),
              [](const auto& a, const auto& b) { return a.first < b.first; });

    // Find minimum satisfying version (MVS)
    for (const auto& [ver, hash] : candidates) {
        if (constraint->satisfiedBy(ver)) {
            return ResolvedDep{
                dep.name,
                dep.git,
                ver.toVString(),
                hash
            };
        }
    }

    std::cerr << "error: no version of '" << dep.git
              << "' satisfies constraint '" << dep.version << "'\n";
    return std::nullopt;
}

} // namespace zo
