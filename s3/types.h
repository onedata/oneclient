/**
* @file types.h
* @author Bartek Kryza
* @copyright (C) 2022-present Onedata.org
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/


#pragma once

#include <drogon/drogon.h>

#include <functional>

namespace one {
namespace s3 {

using HttpResponseCallback =
    std::function<void(const drogon::HttpResponsePtr &)>;

} // namespace s3
} // namespace one