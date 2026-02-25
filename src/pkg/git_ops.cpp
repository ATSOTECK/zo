#include "pkg/git_ops.h"

#include <git2.h>

#include <cstring>
#include <iostream>

namespace zo {

GitOps::GitOps() {
    if (git_libgit2_init() > 0) {
        initialized_ = true;
    }
}

GitOps::~GitOps() {
    if (initialized_) {
        git_libgit2_shutdown();
    }
}

std::vector<RemoteTag> GitOps::listRemoteTags(const std::string& url) {
    std::vector<RemoteTag> tags;

    std::string fullUrl = "https://" + url;

    git_remote* remote = nullptr;
    int err = git_remote_create_detached(&remote, fullUrl.c_str());
    if (err < 0) {
        const git_error* e = git_error_last();
        std::cerr << "error: failed to create remote: "
                  << (e ? e->message : "unknown") << "\n";
        return tags;
    }

    // Connect for fetch
    git_remote_callbacks callbacks = GIT_REMOTE_CALLBACKS_INIT;
    err = git_remote_connect(remote, GIT_DIRECTION_FETCH, &callbacks, nullptr, nullptr);
    if (err < 0) {
        const git_error* e = git_error_last();
        std::cerr << "error: failed to connect to remote: "
                  << (e ? e->message : "unknown") << "\n";
        git_remote_free(remote);
        return tags;
    }

    // List refs
    const git_remote_head** refs = nullptr;
    size_t refCount = 0;
    err = git_remote_ls(&refs, &refCount, remote);
    if (err < 0) {
        git_remote_disconnect(remote);
        git_remote_free(remote);
        return tags;
    }

    const char* tagPrefix = "refs/tags/";
    size_t prefixLen = std::strlen(tagPrefix);

    for (size_t i = 0; i < refCount; i++) {
        const char* name = refs[i]->name;
        if (std::strncmp(name, tagPrefix, prefixLen) != 0) continue;

        std::string tagName = name + prefixLen;

        // Skip peeled refs (e.g. refs/tags/v1.0^{})
        if (tagName.size() >= 3 && tagName.substr(tagName.size() - 3) == "^{}") {
            continue;
        }

        char oidHex[GIT_OID_SHA1_HEXSIZE + 1];
        git_oid_tostr(oidHex, sizeof(oidHex), &refs[i]->oid);

        tags.push_back({tagName, std::string(oidHex)});
    }

    git_remote_disconnect(remote);
    git_remote_free(remote);

    return tags;
}

} // namespace zo
