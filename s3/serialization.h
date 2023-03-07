/**
 * @file serialization.h
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include <aws/s3/model/CreateMultipartUploadResult.h>
#include <aws/s3/model/CompleteMultipartUploadResult.h>
#include <aws/s3/model/ListBucketsResult.h>
#include <aws/s3/model/ListObjectsV2Result.h>
#include <aws/s3/model/ListObjectsResult.h>
#include <aws/s3/model/ListPartsResult.h>
#include <aws/s3/model/DeleteObjectsResult.h>

#include <Poco/DOM/AutoPtr.h>
#include <Poco/DOM/Document.h>
#include <Poco/DOM/Element.h>
#include <Poco/DOM/Text.h>

namespace one {
namespace s3 {

using XMLElement = Poco::XML::Element;
template <typename T> using XMLPtr = Poco::XML::AutoPtr<T>;

using XMLText = Poco::XML::Text;
using XMLDocument = Poco::XML::Document;

template <typename T> std::string serialize(const T &);

template <typename T> void serialize(XMLPtr<XMLElement>, const T &);

template <>
std::string serialize<Aws::S3::Model::ListBucketsResult>(
    const Aws::S3::Model::ListBucketsResult &);

template <>
std::string serialize<Aws::S3::Model::ListObjectsResult>(
    const Aws::S3::Model::ListObjectsResult &);

template <>
std::string serialize<Aws::S3::Model::ListObjectsV2Result>(
    const Aws::S3::Model::ListObjectsV2Result &);

template <>
std::string serialize<Aws::S3::Model::CreateMultipartUploadResult>(
    const Aws::S3::Model::CreateMultipartUploadResult &);

template <>
std::string serialize<Aws::S3::Model::CompleteMultipartUploadResult>(
    const Aws::S3::Model::CompleteMultipartUploadResult &);

template <>
std::string serialize<Aws::S3::Model::ListPartsResult>(
    const Aws::S3::Model::ListPartsResult &);

template <>
std::string serialize<Aws::S3::Model::DeleteObjectsResult>(
    const Aws::S3::Model::DeleteObjectsResult &);

template <>
void serialize(XMLPtr<XMLElement> parent,
    const Aws::Vector<Aws::S3::Model::Bucket> &buckets);

template <>
void serialize(XMLPtr<XMLElement> parent,
    const Aws::Vector<Aws::S3::Model::CommonPrefix> &prefixes);

template <>
void serialize(XMLPtr<XMLElement> parent,
    const Aws::Vector<Aws::S3::Model::Object> &objects);

template <>
void serialize(
    XMLPtr<XMLElement> parent, const Aws::Vector<Aws::S3::Model::Part> &parts);
}

}