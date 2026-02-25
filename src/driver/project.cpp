#include "driver/project.h"
#include "driver/compiler.h"
#include "common/diagnostics.h"
#include "pkg/build_config.h"
#include "pkg/git_ops.h"
#include "pkg/lock_file.h"
#include "pkg/resolver.h"

#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

namespace fs = std::filesystem;

namespace zo {

bool Project::init(const std::string& dir, const std::string& name) {
    fs::path projectDir = fs::path(dir) / name;

    if (fs::exists(projectDir)) {
        std::cerr << "error: directory '" << name << "' already exists\n";
        return false;
    }

    fs::create_directories(projectDir);

    // Write build.toml
    {
        std::ofstream f(projectDir / "build.toml");
        f << "[package]\n"
          << "name = \"" << name << "\"\n"
          << "version = \"0.1.0\"\n"
          << "zo = \"0.1\"\n"
          << "\n"
          << "[dependencies]\n"
          << "\n"
          << "[go]\n"
          << "version = \"1.21\"\n";
    }

    // Write main.zo
    {
        std::ofstream f(projectDir / "main.zo");
        f << "package main\n"
          << "\n"
          << "import fmt\n"
          << "\n"
          << "func main() {\n"
          << "\tfmt::Println(\"Hello from zo!\")\n"
          << "}\n";
    }

    std::cout << "Created project '" << name << "'\n";
    return true;
}

std::vector<std::string> Project::findZoFiles(const std::string& dir) {
    std::vector<std::string> files;
    for (const auto& entry : fs::directory_iterator(dir)) {
        if (entry.path().extension() == ".zo") {
            files.push_back(entry.path().string());
        }
    }
    return files;
}

int Project::build(const std::string& dir) {
    auto zoFiles = findZoFiles(dir);
    if (zoFiles.empty()) {
        std::cerr << "error: no .zo files found in " << dir << "\n";
        return 1;
    }

    // Load build config (fallback to defaults if no build.toml)
    BuildConfig config;
    auto loaded = BuildConfig::load(dir);
    if (loaded) {
        config = std::move(*loaded);
    } else {
        config.package.name = "zo-project";
        config.go.version = "1.21";
    }

    // Build import map from dependencies
    auto importMap = config.buildImportMap();

    DiagnosticEngine diag;
    Compiler compiler(diag);
    compiler.setImportMap(importMap);

    // Create build directory
    fs::path buildDir = fs::path(dir) / ".zo-build";
    fs::create_directories(buildDir);

    // Compile each .zo file
    for (const auto& zoFile : zoFiles) {
        std::ifstream in(zoFile);
        if (!in) {
            std::cerr << "error: cannot open " << zoFile << "\n";
            return 1;
        }

        std::ostringstream buf;
        buf << in.rdbuf();
        std::string source = buf.str();

        std::string filename = fs::path(zoFile).filename().string();
        std::string goCode = compiler.compile(source, filename);

        if (diag.hasErrors()) {
            return 1;
        }

        // Write .go file
        std::string goFilename = fs::path(filename).stem().string() + ".go";
        std::ofstream out(buildDir / goFilename);
        out << goCode;
    }

    // Resolve dependencies and update lock file if we have deps
    if (!config.dependencies.empty()) {
        GitOps gitOps;
        Resolver resolver(gitOps);
        auto lock = LockFile::load(dir);

        auto resolved = resolver.resolve(config.dependencies, lock);
        if (resolved.empty() && !config.dependencies.empty()) {
            return 1;  // resolution failed
        }

        // Update lock file
        LockFile newLock;
        for (const auto& dep : resolved) {
            newLock.setDependency({dep.name, dep.git, dep.version, dep.commitHash});
        }
        newLock.save(dir);
    }

    // Always regenerate go.mod (it's in .zo-build/)
    generateGoMod(buildDir.string(), config);

    // Run go mod tidy if we have dependencies
    if (!config.dependencies.empty()) {
        std::string tidyCmd = "cd \"" + buildDir.string() + "\" && go mod tidy";
        int tidyResult = std::system(tidyCmd.c_str());
        if (tidyResult != 0) {
            std::cerr << "error: go mod tidy failed\n";
            return 1;
        }
    }

    // Run go build
    return goBuild(buildDir.string(), (fs::path(dir) / "output").string());
}

void Project::generateGoMod(const std::string& buildDir, const BuildConfig& config) {
    fs::path goModPath = fs::path(buildDir) / "go.mod";

    std::ofstream goMod(goModPath);
    goMod << "module " << config.package.name << "\n\n"
          << "go " << config.go.version << "\n";

    if (!config.dependencies.empty()) {
        goMod << "\nrequire (\n";
        for (const auto& dep : config.dependencies) {
            // Use the git path as Go module path
            goMod << "\t" << dep.git << " " << dep.version << "\n";
        }
        goMod << ")\n";
    }
}

int Project::goBuild(const std::string& buildDir, const std::string& outputBinary) {
    std::string cmd = "cd \"" + buildDir + "\" && go build -o \"" + outputBinary + "\" .";
    int result = std::system(cmd.c_str());
    if (result != 0) {
        std::cerr << "error: go build failed\n";
    }
    return result;
}

int Project::run(const std::string& dir) {
    int buildResult = build(dir);
    if (buildResult != 0) return buildResult;

    fs::path binary = fs::path(dir) / "output";
    if (!fs::exists(binary)) {
        std::cerr << "error: binary not found after build\n";
        return 1;
    }

    std::string cmd = "\"" + binary.string() + "\"";
    return std::system(cmd.c_str());
}

int Project::get(const std::string& dir, const std::string& package,
                 const std::string& version) {
    // Load or create build config
    BuildConfig config;
    auto loaded = BuildConfig::load(dir);
    if (loaded) {
        config = std::move(*loaded);
    } else {
        std::cerr << "error: no build.toml found in " << dir << "\n";
        return 1;
    }

    // Derive short name from last path segment
    std::string shortName = package;
    auto lastSlash = package.rfind('/');
    if (lastSlash != std::string::npos) {
        shortName = package.substr(lastSlash + 1);
    }

    // Discover versions via git tags
    GitOps gitOps;
    auto remoteTags = gitOps.listRemoteTags(package);

    if (remoteTags.empty()) {
        std::cerr << "error: no tags found for '" << package << "'\n";
        return 1;
    }

    // Parse all tags as semver
    std::vector<std::pair<SemVer, std::string>> candidates;
    for (const auto& tag : remoteTags) {
        auto ver = SemVer::parse(tag.name);
        if (ver) {
            candidates.push_back({*ver, tag.commitHash});
        }
    }

    if (candidates.empty()) {
        std::cerr << "error: no valid semver tags found for '" << package << "'\n";
        return 1;
    }

    // Sort ascending
    std::sort(candidates.begin(), candidates.end(),
              [](const auto& a, const auto& b) { return a.first < b.first; });

    SemVer selectedVer;
    std::string selectedHash;

    if (!version.empty()) {
        // Use specified version constraint
        auto constraint = VersionConstraint::parse(version);
        if (!constraint) {
            std::cerr << "error: invalid version constraint '" << version << "'\n";
            return 1;
        }

        bool found = false;
        for (const auto& [ver, hash] : candidates) {
            if (constraint->satisfiedBy(ver)) {
                selectedVer = ver;
                selectedHash = hash;
                found = true;
                break;
            }
        }

        if (!found) {
            std::cerr << "error: no version of '" << package
                      << "' satisfies constraint '" << version << "'\n";
            return 1;
        }
    } else {
        // Pick latest
        selectedVer = candidates.back().first;
        selectedHash = candidates.back().second;
    }

    // Add or update dependency
    Dependency dep;
    dep.name = shortName;
    dep.git = package;
    dep.version = selectedVer.toVString();
    config.addDependency(dep);
    config.save(dir);

    // Update lock file
    LockFile lock;
    auto existingLock = LockFile::load(dir);
    if (existingLock) lock = std::move(*existingLock);

    lock.setDependency({shortName, package, selectedVer.toVString(), selectedHash});
    lock.save(dir);

    std::cout << "Added " << package << " " << selectedVer.toVString() << "\n";
    return 0;
}

} // namespace zo
