#pragma once

#include <string>
#include <vector>

namespace zo {

struct BuildConfig;

class Project {
public:
    // Initialize a new zo project in the given directory.
    static bool init(const std::string& dir, const std::string& name);

    // Build a zo project. Returns 0 on success.
    static int build(const std::string& dir);

    // Build and run a zo project. Returns the exit code.
    static int run(const std::string& dir);

    // Add a dependency: zo get <package> [--version <ver>]
    static int get(const std::string& dir, const std::string& package,
                   const std::string& version = "");

private:
    // Find all .zo files in the directory.
    static std::vector<std::string> findZoFiles(const std::string& dir);

    // Generate go.mod with require directives into buildDir.
    static void generateGoMod(const std::string& buildDir, const BuildConfig& config);

    // Write generated Go code and run `go build`.
    static int goBuild(const std::string& buildDir, const std::string& outputBinary);
};

} // namespace zo
