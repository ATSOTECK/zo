#include "driver/project.h"
#include "driver/compiler.h"
#include "common/diagnostics.h"

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

    DiagnosticEngine diag;
    Compiler compiler(diag);

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

    // Generate go.mod if it doesn't exist
    fs::path goModPath = buildDir / "go.mod";
    if (!fs::exists(goModPath)) {
        // Try to read package name from build.toml
        std::string moduleName = "zo-project";
        fs::path buildToml = fs::path(dir) / "build.toml";
        if (fs::exists(buildToml)) {
            std::ifstream f(buildToml);
            std::string line;
            while (std::getline(f, line)) {
                if (line.find("name") != std::string::npos && line.find("=") != std::string::npos) {
                    auto pos = line.find('"');
                    auto end = line.rfind('"');
                    if (pos != std::string::npos && end != pos) {
                        moduleName = line.substr(pos + 1, end - pos - 1);
                    }
                    break;
                }
            }
        }

        std::ofstream goMod(goModPath);
        goMod << "module " << moduleName << "\n\ngo 1.21\n";
    }

    // Run go build
    return goBuild(buildDir.string(), (fs::path(dir) / "output").string());
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

} // namespace zo
