#pragma once

#include <string>
#include <vector>

namespace zo {

struct RemoteTag {
    std::string name;      // e.g. "v1.2.3"
    std::string commitHash;
};

class GitOps {
public:
    GitOps();
    ~GitOps();

    GitOps(const GitOps&) = delete;
    GitOps& operator=(const GitOps&) = delete;

    // List tags from a remote repository.
    // `url` is like "github.com/go-chi/chi" (https:// is prepended).
    std::vector<RemoteTag> listRemoteTags(const std::string& url);

private:
    bool initialized_ = false;
};

} // namespace zo
