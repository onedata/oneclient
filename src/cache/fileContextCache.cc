#include "fileContextCache.h"

#include <stdexcept>
#include <string>

namespace one {
namespace client {

FileContext FileContextCache::get(FuseFileHandle fh)
{
    ConstAccessor constAcc;
    if (!m_cache.find(constAcc, fh))
        throw std::out_of_range{"no file context for " + std::to_string(fh)};

    return constAcc->second;
}

void FileContextCache::get(Accessor &acc, FuseFileHandle fh)
{
    if (!m_cache.find(acc, fh))
        throw std::out_of_range{"no file context for " + std::to_string(fh)};
}

void FileContextCache::create(Accessor &acc)
{
    auto fh = m_nextHandle++;
    while (!m_cache.insert(acc, fh))
        fh = m_nextHandle++;
}

void FileContextCache::erase(Accessor &acc) { m_cache.erase(acc); }

} // namespace one
} // namespace client
