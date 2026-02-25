#include "pkg/semver.h"

#include <algorithm>
#include <charconv>

namespace zo {

// ─── SemVer ─────────────────────────────────────────────────────────

static bool tryParseInt(const std::string& s, int& out) {
    auto [ptr, ec] = std::from_chars(s.data(), s.data() + s.size(), out);
    return ec == std::errc{} && ptr == s.data() + s.size();
}

std::optional<SemVer> SemVer::parse(const std::string& s) {
    if (s.empty()) return std::nullopt;

    std::string input = s;
    // Strip leading 'v' or 'V'
    if (input[0] == 'v' || input[0] == 'V') {
        input = input.substr(1);
    }
    if (input.empty()) return std::nullopt;

    // Split off prerelease at first '-'
    std::string prerelease;
    auto dashPos = input.find('-');
    if (dashPos != std::string::npos) {
        prerelease = input.substr(dashPos + 1);
        input = input.substr(0, dashPos);
    }

    // Split major.minor.patch
    SemVer ver;
    ver.prerelease = prerelease;

    auto dot1 = input.find('.');
    if (dot1 == std::string::npos) {
        // major only, e.g. "5"
        if (!tryParseInt(input, ver.major)) return std::nullopt;
        return ver;
    }

    std::string majorStr = input.substr(0, dot1);
    if (!tryParseInt(majorStr, ver.major)) return std::nullopt;

    auto dot2 = input.find('.', dot1 + 1);
    if (dot2 == std::string::npos) {
        // major.minor only
        std::string minorStr = input.substr(dot1 + 1);
        if (!tryParseInt(minorStr, ver.minor)) return std::nullopt;
        return ver;
    }

    std::string minorStr = input.substr(dot1 + 1, dot2 - dot1 - 1);
    std::string patchStr = input.substr(dot2 + 1);
    if (!tryParseInt(minorStr, ver.minor)) return std::nullopt;
    if (!tryParseInt(patchStr, ver.patch)) return std::nullopt;

    return ver;
}

static int comparePrerelease(const std::string& a, const std::string& b) {
    // Both empty → equal
    if (a.empty() && b.empty()) return 0;
    // Empty prerelease > any prerelease (1.0.0 > 1.0.0-beta)
    if (a.empty()) return 1;
    if (b.empty()) return -1;
    // Lexicographic comparison for simplicity
    if (a < b) return -1;
    if (a > b) return 1;
    return 0;
}

bool SemVer::operator<(const SemVer& o) const {
    if (major != o.major) return major < o.major;
    if (minor != o.minor) return minor < o.minor;
    if (patch != o.patch) return patch < o.patch;
    return comparePrerelease(prerelease, o.prerelease) < 0;
}

bool SemVer::operator>(const SemVer& o) const  { return o < *this; }
bool SemVer::operator<=(const SemVer& o) const { return !(o < *this); }
bool SemVer::operator>=(const SemVer& o) const { return !(*this < o); }
bool SemVer::operator==(const SemVer& o) const {
    return major == o.major && minor == o.minor && patch == o.patch
           && prerelease == o.prerelease;
}
bool SemVer::operator!=(const SemVer& o) const { return !(*this == o); }

std::string SemVer::toString() const {
    std::string s = std::to_string(major) + "." + std::to_string(minor) + "." + std::to_string(patch);
    if (!prerelease.empty()) s += "-" + prerelease;
    return s;
}

std::string SemVer::toVString() const {
    return "v" + toString();
}

// ─── VersionConstraint ──────────────────────────────────────────────

std::optional<VersionConstraint> VersionConstraint::parse(const std::string& s) {
    if (s.empty()) return std::nullopt;

    VersionConstraint c;
    std::string rest = s;

    if (rest[0] == '^') {
        c.op = ConstraintOp::Caret;
        rest = rest.substr(1);
    } else if (rest[0] == '~') {
        c.op = ConstraintOp::Tilde;
        rest = rest.substr(1);
    } else if (rest.size() >= 2 && rest.substr(0, 2) == ">=") {
        c.op = ConstraintOp::Gte;
        rest = rest.substr(2);
    } else if (rest.size() >= 2 && rest.substr(0, 2) == "<=") {
        c.op = ConstraintOp::Lte;
        rest = rest.substr(2);
    } else if (rest[0] == '>') {
        c.op = ConstraintOp::Gt;
        rest = rest.substr(1);
    } else if (rest[0] == '<') {
        c.op = ConstraintOp::Lt;
        rest = rest.substr(1);
    } else {
        c.op = ConstraintOp::Exact;
    }

    auto ver = SemVer::parse(rest);
    if (!ver) return std::nullopt;
    c.base = *ver;
    return c;
}

bool VersionConstraint::satisfiedBy(const SemVer& ver) const {
    switch (op) {
        case ConstraintOp::Exact:
            return ver == base;

        case ConstraintOp::Caret: {
            // ^1.2.3 → >=1.2.3, <2.0.0
            // ^0.2.3 → >=0.2.3, <0.3.0
            // ^0.0.3 → >=0.0.3, <0.0.4
            if (ver < base) return false;
            if (base.major != 0) {
                return ver.major == base.major;
            }
            if (base.minor != 0) {
                return ver.major == 0 && ver.minor == base.minor;
            }
            return ver.major == 0 && ver.minor == 0 && ver.patch == base.patch;
        }

        case ConstraintOp::Tilde:
            // ~1.2.3 → >=1.2.3, <1.3.0
            if (ver < base) return false;
            return ver.major == base.major && ver.minor == base.minor;

        case ConstraintOp::Gte: return ver >= base;
        case ConstraintOp::Lte: return ver <= base;
        case ConstraintOp::Gt:  return ver > base;
        case ConstraintOp::Lt:  return ver < base;
    }
    return false;
}

// ─── selectVersion ──────────────────────────────────────────────────

std::optional<SemVer> selectVersion(const std::vector<SemVer>& available,
                                     const VersionConstraint& constraint) {
    // Sort ascending, pick first satisfying (MVS-like: minimum satisfying version)
    auto sorted = available;
    std::sort(sorted.begin(), sorted.end());

    for (const auto& v : sorted) {
        if (constraint.satisfiedBy(v)) {
            return v;
        }
    }
    return std::nullopt;
}

} // namespace zo
