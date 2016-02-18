#include "macaroons.hpp"

#include <cassert>
#include <iostream>
#include <string>
#include <sstream>
#include <iomanip>
#include <ctime>

using namespace macaroons;

std::string hexEncode(const std::vector<unsigned char> &data)
{
    std::stringstream stream;
    stream.fill('0');

    for (auto c : data)
        stream << std::hex << std::setw(2) << static_cast<unsigned int>(c);

    return stream.str();
}

void usingThirdPartyCaveats(Verifier &V);

int main()
{
    auto secret = "this is our super secret key; only we should know it";
    auto pub = "we used our secret key";
    auto location = "http://mybank/";

    Macaroon M{location, secret, pub};

    assert(M.identifier() == pub);
    assert(M.location() == location);
    assert(hexEncode(M.signature()) ==
        "e3d9e02908526c4c0039ae15114115d97fdd68bf2ba379b342aaf0f617d0552f");
    assert(M.serialize() == "MDAxY2xvY2F0aW9uIGh0dHA6Ly9teWJhbmsvCjAwMjZpZGVudG"
                            "lmaWVyIHdlIHVzZWQgb3VyIHNlY3JldCBrZXkKMDAyZnNpZ25h"
                            "dHVyZSDj2eApCFJsTAA5rhURQRXZf91ovyujebNCqvD2F9BVLw"
                            "o");

    std::cout << M.inspect() << std::endl;

    M = M.addFirstPartyCaveat("account = 3735928559");

    std::cout << std::endl;
    std::cout << M.inspect() << std::endl;

    M = M.addFirstPartyCaveat("time < 2020-01-01T00:00");
    assert(hexEncode(M.signature()) ==
        "b5f06c8c8ef92f6c82c6ff282cd1f8bd1849301d09a2db634ba182536a611c49");

    M = M.addFirstPartyCaveat("email = alice@example.org");
    assert(hexEncode(M.signature()) ==
        "ddf553e46083e55b8d71ab822be3d8fcf21d6bf19c40d617bb9fb438934474b6");

    std::cout << std::endl;
    std::cout << M.inspect() << std::endl;

    auto msg = M.serialize();
    assert(M == Macaroon::deserialize(msg));

    M = Macaroon::deserialize(msg);

    std::cout << std::endl;
    std::cout << M.inspect() << std::endl;

    Verifier V;

    try {
        V.verify(M, secret);
    }
    catch (exception::NotAuthorized &e) {
        std::cout << e.what() << std::endl;
    }

    assert(!V.verifyUnsafe(M, secret));

    V.satisfyExact("account = 3735928559");
    V.satisfyExact("email = alice@example.org");
    V.satisfyExact("IP = 127.0.0.1");
    V.satisfyExact("browser = Chrome");
    V.satisfyExact("action = deposit");

    auto checkTime = [](const std::string &caveat) {
        if (caveat.find("time < ") != 0)
            return false;

        std::tm tm;
        std::istringstream ss(caveat.substr(7));
        ss >> std::get_time(&tm, "%Y-%m-%dT%H:%M");
        if (ss.fail())
            return false;

        auto t = std::mktime(&tm);
        return std::time(nullptr) < t;
    };

    assert(checkTime("time < 2020-01-01T00:00"));
    assert(!checkTime("time < 2014-01-01T00:00"));
    assert(!checkTime("account = 3735928559"));

    V.satisfyGeneral(std::move(checkTime));
    assert(V.verifyUnsafe(M, secret));

    auto N = M.addFirstPartyCaveat("action = deposit");
    assert(V.verifyUnsafe(N, secret));

    // Unknown caveat
    N = M.addFirstPartyCaveat("OS = Windows XP");
    assert(!V.verifyUnsafe(N, secret));

    // False caveat
    N = M.addFirstPartyCaveat("time < 2014-01-01T00:00");
    assert(!V.verifyUnsafe(N, secret));

    // Bad secret
    assert(!V.verifyUnsafe(M, "this is not the secret we were looking for"));

    // Incompetent hackers trying to change the signature
    N = Macaroon::deserialize(
        "MDAxY2xvY2F0aW9uIGh0dHA6Ly9teWJhbmsvCjAwMjZpZGVudGlmaWVyIHdlIHVzZWQgb3"
        "VyIHNl\nY3JldCBrZXkKMDAxZGNpZCBhY2NvdW50ID0gMzczNTkyODU1OQowMDIwY2lkIH"
        "RpbWUgPCAyMDIw\nLTAxLTAxVDAwOjAwCjAwMjJjaWQgZW1haWwgPSBhbGljZUBleGFtcG"
        "xlLm9yZwowMDJmc2lnbmF0\ndXJlID8f19FL+bkC9p/"
        "aoMmIecC7GxdOcLVyUnrv6lJMM7NSCg==\n");

    std::cout << std::endl;
    std::cout << N.inspect() << std::endl;

    assert(M.signature() != N.signature());
    assert(!V.verifyUnsafe(N, secret));

    usingThirdPartyCaveats(V);
}

void usingThirdPartyCaveats(Verifier &V)
{
    auto secret =
        "this is a different super-secret key; never use the same secret twice";
    auto pub = "we used our other secret key";
    auto location = "http://mybank/";

    Macaroon M{location, secret, pub};
    M = M.addFirstPartyCaveat("account = 3735928559");

    std::cout << std::endl;
    std::cout << M.inspect() << std::endl;

    // you'll likely want to use a higher entropy source to generate this key
    auto caveatKey = "4; guaranteed random by a fair toss of the dice";
    auto predicate = "user = Alice";

    // sendToAuth(caveat_key, predicate);
    // auto identifier = recvFromAuth();

    auto identifier = "this was how we remind auth of key/pred";
    M = M.addThirdPartyCaveat("http://auth.mybank/", caveatKey, identifier);

    std::cout << std::endl;
    std::cout << M.inspect() << std::endl;

    assert(M.thirdPartyCaveats().size() == 1);
    assert(M.thirdPartyCaveats().front().location() == "http://auth.mybank/");
    assert(M.thirdPartyCaveats().front().identifier() ==
        "this was how we remind auth of key/pred");

    Macaroon D{"http://auth.mybank/", caveatKey, identifier};
    D = D.addFirstPartyCaveat("time < 2020-01-01T00:00");

    std::cout << std::endl;
    std::cout << D.inspect() << std::endl;

    auto DP = M.prepareForRequest(D);

    assert(hexEncode(D.signature()) ==
        "2ed1049876e9d5840950274b579b0770317df54d338d9d3039c7c67d0d91d63c");

    assert(V.verifyUnsafe(M, secret, {DP}));
    assert(!V.verifyUnsafe(M, secret, {D}));
}
