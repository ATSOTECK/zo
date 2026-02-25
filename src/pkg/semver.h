#pragma once

#include <optional>
#include <string>
#include <vector>

namespace zo {

struct SemVer {
    int major = 0;
    int minor = 0;
    int patch = 0;
    std::string prerelease;  // e.g. "beta.1"

    static std::optional<SemVer> parse(const std::string& s);

    bool operator<(const SemVer& o) const;
    bool operator>(const SemVer& o) const;
    bool operator<=(const SemVer& o) const;
    bool operator>=(const SemVer& o) const;
    bool operator==(const SemVer& o) const;
    bool operator!=(const SemVer& o) const;

    std::string toString() const;    // "1.2.3" or "1.2.3-beta.1"
    std::string toVString() const;   // "v1.2.3"
};

enum class ConstraintOp { Exact, Caret, Tilde, Gte, Lte, Gt, Lt };

struct VersionConstraint {
    ConstraintOp op;
    SemVer base;

    static std::optional<VersionConstraint> parse(const std::string& s);

    bool satisfiedBy(const SemVer& ver) const;
};

// Returns the minimum version from `available` that satisfies `constraint`.
std::optional<SemVer> selectVersion(const std::vector<SemVer>& available,
                                     const VersionConstraint& constraint);

} // namespace zo
