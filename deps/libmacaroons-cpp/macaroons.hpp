/**
 * @file macaroons.hpp
 * @copyright Copyright (c) 2015, Konrad Zemek <konrad.zemek@gmail.com>
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 * may be used to endorse or promote products derived from this software without
 * specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef MACAROONS_HPP
#define MACAROONS_HPP

#include "macaroons.h"

#include <algorithm>
#include <forward_list>
#include <functional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>
#include <iomanip>

#define MACAROONS_DEFINE_EXCEPTION(NAME, WHAT, CODE)                           \
    class NAME : public Exception {                                            \
    public:                                                                    \
        NAME()                                                                 \
            : Exception{"macaroons: " WHAT, CODE}                              \
        {                                                                      \
        }                                                                      \
    }

namespace macaroons {

constexpr std::size_t MAX_STRLEN = MACAROON_MAX_STRLEN;
constexpr std::size_t MAX_CAVEATS = MACAROON_MAX_CAVEATS;
constexpr std::size_t SUGGESTED_SECRET_LENGTH =
    MACAROON_SUGGESTED_SECRET_LENGTH;

namespace exception {
class Exception : public std::runtime_error {
public:
    Exception(int code_)
        : std::runtime_error{"operation failed unexpectedly"}
        , m_code{code_}
    {
    }

    Exception(const char *what_, int code_)
        : std::runtime_error{what_}
        , m_code{code_}
    {
    }

    int code() const { return m_code; }

private:
    const int m_code;
};

MACAROONS_DEFINE_EXCEPTION(
    BufferTooSmall, "buffer too small", MACAROON_BUF_TOO_SMALL);
MACAROONS_DEFINE_EXCEPTION(
    Cycle, "discharge caveats form a cycle", MACAROON_CYCLE);
MACAROONS_DEFINE_EXCEPTION(
    HashFailed, "HMAC function failed", MACAROON_HASH_FAILED);
MACAROONS_DEFINE_EXCEPTION(
    NotAuthorized, "not authorized", MACAROON_NOT_AUTHORIZED);
MACAROONS_DEFINE_EXCEPTION(
    NoJSONSupport, "JSON macaroons not supported", MACAROON_NO_JSON_SUPPORT);
MACAROONS_DEFINE_EXCEPTION(
    TooManyCaveats, "too many caveats", MACAROON_TOO_MANY_CAVEATS);
MACAROONS_DEFINE_EXCEPTION(Invalid, "macaroon invalid", MACAROON_INVALID);

inline void throwOnError(const macaroon_returncode err)
{
    switch (err) {
        case MACAROON_SUCCESS:
            break;
        case MACAROON_BUF_TOO_SMALL:
            throw BufferTooSmall{};
        case MACAROON_CYCLE:
            throw Cycle{};
        case MACAROON_HASH_FAILED:
            throw HashFailed{};
        case MACAROON_INVALID:
            throw Invalid{};
        case MACAROON_NOT_AUTHORIZED:
            throw NotAuthorized{};
        case MACAROON_NO_JSON_SUPPORT:
            throw NoJSONSupport{};
        case MACAROON_TOO_MANY_CAVEATS:
            throw TooManyCaveats{};
        case MACAROON_OUT_OF_MEMORY:
            throw std::bad_alloc{};
        default:
            throw Exception{err};
    }
}

} // namespace exception

namespace detail {

class Stringizer {
public:
    operator std::string() const { return {m_data, m_size}; }
    operator size_t *() { return &m_size; }
    operator const unsigned char **()
    {
        return reinterpret_cast<const unsigned char **>(&m_data);
    }

private:
    const char *m_data = nullptr;
    size_t m_size = 0;
};

inline const unsigned char *cast(const std::string &str)
{
    return reinterpret_cast<const unsigned char *>(str.c_str());
}

} // namespace detail

class ThirdPartyCaveat {
public:
    ThirdPartyCaveat(std::string location_, std::string identifier_)
        : m_location{std::move(location_)}
        , m_identifier{std::move(identifier_)}
    {
    }

    const std::string &location() const { return m_location; }
    const std::string &identifier() const { return m_identifier; }

private:
    std::string m_location;
    std::string m_identifier;
};

class Macaroon {
    friend class Verifier;

public:
    Macaroon(Macaroon &&o) { *this = std::move(o); }

    Macaroon &operator=(Macaroon &&o)
    {
        std::swap(m_macaroon, o.m_macaroon);
        return *this;
    }

    Macaroon(const std::string &location, const std::string &key,
        const std::string &id)
    {
        macaroon_returncode err = MACAROON_SUCCESS;
        m_macaroon = macaroon_create(detail::cast(location), location.size(),
            detail::cast(key), key.size(), detail::cast(id), id.size(), &err);
        exception::throwOnError(err);
    }

    Macaroon(const Macaroon &o) { *this = o; }

    Macaroon &operator=(const Macaroon &o)
    {
        if (this == &o)
            return *this;

        macaroon_returncode err = MACAROON_SUCCESS;
        auto macaroon = macaroon_copy(o.m_macaroon, &err);
        exception::throwOnError(err);

        m_macaroon = macaroon;
        return *this;
    }

    ~Macaroon()
    {
        if (m_macaroon)
            macaroon_destroy(m_macaroon);
    }

    Macaroon addFirstPartyCaveat(const std::string &predicate) const
    {
        macaroon_returncode err = MACAROON_SUCCESS;
        auto macaroon = macaroon_add_first_party_caveat(
            m_macaroon, detail::cast(predicate), predicate.size(), &err);

        exception::throwOnError(err);
        return {macaroon};
    }

    Macaroon addThirdPartyCaveat(const std::string &location,
        const std::string &key, const std::string &id) const
    {
        macaroon_returncode err = MACAROON_SUCCESS;
        auto macaroon = macaroon_add_third_party_caveat(m_macaroon,
            detail::cast(location), location.size(), detail::cast(key),
            key.size(), detail::cast(id), id.size(), &err);

        exception::throwOnError(err);
        return {macaroon};
    }

    std::vector<ThirdPartyCaveat> thirdPartyCaveats() const
    {
        const auto size = macaroon_num_third_party_caveats(m_macaroon);

        std::vector<ThirdPartyCaveat> caveats;
        for (std::size_t i = 0; i < size; ++i) {
            detail::Stringizer loc, id;
            macaroon_third_party_caveat(m_macaroon, i, loc, loc, id, id);
            caveats.emplace_back(std::move(loc), std::move(id));
        }

        return caveats;
    }

    Macaroon prepareForRequest(const Macaroon &dispatch)
    {
        macaroon_returncode err = MACAROON_SUCCESS;
        auto macaroon =
            macaroon_prepare_for_request(m_macaroon, dispatch.m_macaroon, &err);
        exception::throwOnError(err);
        return {macaroon};
    }

    std::string location() const
    {
        detail::Stringizer loc;
        macaroon_location(m_macaroon, loc, loc);
        return loc;
    }

    std::string identifier() const
    {
        detail::Stringizer id;
        macaroon_identifier(m_macaroon, id, id);
        return id;
    }

    std::vector<unsigned char> signature() const
    {
        const unsigned char *sig = nullptr;
        size_t sigLen = 0;
        macaroon_signature(m_macaroon, &sig, &sigLen);
        return {sig, sig + sigLen};
    }

    std::string serialize() const
    {
        return serializeGeneric(
            macaroon_serialize_size_hint, macaroon_serialize);
    }

    std::string serializeJSON() const
    {
        return serializeGeneric(
            macaroon_serialize_json_size_hint, macaroon_serialize_json);
    }

    std::string inspect() const
    {
        return serializeGeneric(macaroon_inspect_size_hint, macaroon_inspect);
    }

    static Macaroon deserializeJSON(const std::string &data)
    {
        macaroon_returncode err = MACAROON_SUCCESS;
        auto macaroon =
            macaroon_deserialize_json(data.data(), data.size(), &err);
        exception::throwOnError(err);

        return {macaroon};
    }

    static Macaroon deserialize(const std::string &data)
    {
        macaroon_returncode err = MACAROON_SUCCESS;
        auto macaroon = macaroon_deserialize(data.data(), &err);
        exception::throwOnError(err);

        return {macaroon};
    }

    bool operator==(const Macaroon &o)
    {
        return macaroon_cmp(m_macaroon, o.m_macaroon) == 0;
    }

private:
    Macaroon(struct macaroon *m)
        : m_macaroon{m}
    {
    }

    std::string serializeGeneric(size_t (*sizeHintFun)(const struct macaroon *),
        int (*serializeFun)(const struct macaroon *, char *, size_t,
                                     enum macaroon_returncode *)) const
    {
        const auto minSize = sizeHintFun(m_macaroon);
        std::string buffer(minSize, '\0');

        macaroon_returncode err = MACAROON_SUCCESS;
        serializeFun(m_macaroon, &buffer[0], buffer.size(), &err);
        while (err == MACAROON_BUF_TOO_SMALL) {
            buffer.resize(buffer.size() * 2);
            serializeFun(m_macaroon, &buffer[0], buffer.size(), &err);
        }

        exception::throwOnError(err);
        return {&buffer[0]};
    }

    struct macaroon *m_macaroon = nullptr;
};

class Verifier {
public:
    Verifier(const Verifier &) = delete;
    Verifier &operator=(const Verifier &) = delete;

    Verifier(Verifier &&o) { *this = std::move(o); }

    Verifier &operator=(Verifier &&o)
    {
        std::swap(m_verifier, o.m_verifier);
        return *this;
    }

    Verifier()
        : m_verifier{macaroon_verifier_create()}
    {
        if (!m_verifier)
            throw std::bad_alloc{};
    }

    ~Verifier() { macaroon_verifier_destroy(m_verifier); }

    void satisfyExact(const std::string &predicate)
    {
        macaroon_returncode err = MACAROON_SUCCESS;
        macaroon_verifier_satisfy_exact(
            m_verifier, detail::cast(predicate), predicate.size(), &err);
        exception::throwOnError(err);
    }

    void satisfyGeneral(
        std::function<bool(const std::string &predicate)> generalCheck)
    {
        m_generalCheck.emplace_front(std::move(generalCheck));

        try {
            macaroon_returncode err = MACAROON_SUCCESS;
            macaroon_verifier_satisfy_general(m_verifier,
                [](void *f, const unsigned char *predicate, size_t predSize) {
                    auto &callback = *static_cast<decltype(generalCheck) *>(f);
                    auto pred = reinterpret_cast<const char *>(predicate);
                    return callback({pred, predSize}) ? 0 : 1;
                },
                &m_generalCheck.front(), &err);
            exception::throwOnError(err);
        }
        catch (...) {
            m_generalCheck.pop_front();
            throw;
        }
    }

    bool verifyUnsafe(const Macaroon &macaroon, const std::string &key,
        const std::vector<Macaroon> &dispatches = {})
    {
        std::vector<struct macaroon *> disp;
        disp.reserve(dispatches.size());
        std::transform(dispatches.begin(), dispatches.end(),
            std::back_inserter(disp),
            [](const Macaroon &mc) { return mc.m_macaroon; });

        macaroon_returncode err = MACAROON_SUCCESS;
        macaroon_verify(m_verifier, macaroon.m_macaroon, detail::cast(key),
            key.size(), disp.data(), disp.size(), &err);

        if (err == MACAROON_NOT_AUTHORIZED)
            return false;

        exception::throwOnError(err);
        return true;
    }

    void verify(const Macaroon &macaroon, const std::string &key,
        const std::vector<Macaroon> &dispatches = {})
    {
        if (!verifyUnsafe(macaroon, key, dispatches))
            throw exception::NotAuthorized{};
    }

private:
    struct macaroon_verifier *m_verifier = nullptr;
    std::forward_list<std::function<bool(const std::string &predicate)>>
        m_generalCheck;
};

} // namespace macaroons

#undef MACAROONS_DEFINE_EXCEPTION

#endif // MACAROONS_HPP
