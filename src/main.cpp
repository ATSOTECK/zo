#include "driver/project.h"

#include <cstring>
#include <filesystem>
#include <iostream>
#include <string>

namespace fs = std::filesystem;

static void printUsage() {
    std::cout << "Usage: zo <command> [arguments]\n"
              << "\n"
              << "Commands:\n"
              << "  build                     Compile the current project\n"
              << "  run                       Compile and run the current project\n"
              << "  init <name>               Create a new zo project\n"
              << "  get <pkg> [--version ver]  Add a dependency\n"
              << "  version                   Print the zo version\n"
              << "  help                      Show this help message\n";
}

static void printVersion() {
    std::cout << "zo 0.1.0\n";
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        printUsage();
        return 1;
    }

    std::string cmd = argv[1];

    if (cmd == "help" || cmd == "--help" || cmd == "-h") {
        printUsage();
        return 0;
    }

    if (cmd == "version" || cmd == "--version" || cmd == "-v") {
        printVersion();
        return 0;
    }

    if (cmd == "init") {
        if (argc < 3) {
            std::cerr << "error: 'zo init' requires a project name\n";
            return 1;
        }
        std::string name = argv[2];
        std::string dir = fs::current_path().string();
        return zo::Project::init(dir, name) ? 0 : 1;
    }

    if (cmd == "build") {
        std::string dir = fs::current_path().string();
        return zo::Project::build(dir);
    }

    if (cmd == "run") {
        std::string dir = fs::current_path().string();
        return zo::Project::run(dir);
    }

    if (cmd == "get") {
        if (argc < 3) {
            std::cerr << "error: 'zo get' requires a package name\n";
            return 1;
        }
        std::string package = argv[2];
        std::string version;
        for (int i = 3; i < argc - 1; i++) {
            if (std::strcmp(argv[i], "--version") == 0) {
                version = argv[i + 1];
                break;
            }
        }
        std::string dir = fs::current_path().string();
        return zo::Project::get(dir, package, version);
    }

    std::cerr << "error: unknown command '" << cmd << "'\n";
    printUsage();
    return 1;
}
