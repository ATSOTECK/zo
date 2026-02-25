#include "pkg/build_config.h"

#include "toml++/toml.hpp"

#include <filesystem>
#include <fstream>

namespace fs = std::filesystem;

namespace zo {

std::optional<BuildConfig> BuildConfig::load(const std::string& dir) {
    fs::path path = fs::path(dir) / "build.toml";
    if (!fs::exists(path)) return std::nullopt;

    try {
        auto result = toml::parse_file(path.string());

        BuildConfig config;

        // [package]
        if (auto pkg = result["package"].as_table()) {
            config.package.name = (*pkg)["name"].value_or(std::string(""));
            config.package.version = (*pkg)["version"].value_or(std::string(""));
            config.package.zo = (*pkg)["zo"].value_or(std::string(""));
        }

        // [go]
        if (auto goSec = result["go"].as_table()) {
            config.go.version = (*goSec)["version"].value_or(std::string("1.21"));
        }

        // [dependencies]
        if (auto deps = result["dependencies"].as_table()) {
            for (const auto& [key, val] : *deps) {
                Dependency dep;
                dep.name = std::string(key.str());

                if (auto tbl = val.as_table()) {
                    dep.git = (*tbl)["git"].value_or(std::string(""));
                    dep.version = (*tbl)["version"].value_or(std::string(""));
                    dep.path = (*tbl)["path"].value_or(std::string(""));
                } else if (auto str = val.as_string()) {
                    // Simple form: rage = "github.com/ATSOTECK/rage"
                    dep.git = str->get();
                }

                config.dependencies.push_back(std::move(dep));
            }
        }

        return config;
    } catch (const toml::parse_error&) {
        return std::nullopt;
    }
}

bool BuildConfig::save(const std::string& dir) const {
    fs::path path = fs::path(dir) / "build.toml";

    std::ofstream out(path);
    if (!out) return false;

    out << "[package]\n"
        << "name = \"" << package.name << "\"\n"
        << "version = \"" << package.version << "\"\n"
        << "zo = \"" << package.zo << "\"\n"
        << "\n"
        << "[dependencies]\n";

    for (const auto& dep : dependencies) {
        out << dep.name << " = { git = \"" << dep.git << "\"";
        if (!dep.version.empty()) {
            out << ", version = \"" << dep.version << "\"";
        }
        if (!dep.path.empty()) {
            out << ", path = \"" << dep.path << "\"";
        }
        out << " }\n";
    }

    out << "\n[go]\n"
        << "version = \"" << go.version << "\"\n";

    return out.good();
}

std::unordered_map<std::string, std::string> BuildConfig::buildImportMap() const {
    std::unordered_map<std::string, std::string> map;

    for (const auto& dep : dependencies) {
        // Full Go import path = git + optional sub-path
        std::string fullPath = dep.git;
        if (!dep.path.empty()) {
            fullPath += "/" + dep.path;
        }
        map[dep.name] = fullPath;
    }

    return map;
}

void BuildConfig::addDependency(const Dependency& dep) {
    // Update existing or add new
    for (auto& existing : dependencies) {
        if (existing.name == dep.name) {
            existing = dep;
            return;
        }
    }
    dependencies.push_back(dep);
}

} // namespace zo
