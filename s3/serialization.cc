/**
 * @file serialization.cc
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "serialization.h"

#include "logging.h"

#include <Poco/DOM/AutoPtr.h>
#include <Poco/DOM/DOMException.h>
#include <Poco/DOM/DOMWriter.h>
#include <Poco/UTF8Encoding.h>
#include <Poco/XML/XMLWriter.h>
#include <fmt/format.h>

#include <sstream>

namespace one {
namespace s3 {

std::string toString(Poco::XML::AutoPtr<XMLDocument> doc)
{
    std::string result;
    Poco::XML::DOMWriter writer;
    writer.setOptions(Poco::XML::XMLWriter::CANONICAL_XML);
    auto encoding = Poco::UTF8Encoding();
    writer.setEncoding(encoding.canonicalName(), encoding);
    std::ostringstream strstr;

    try {
        writer.writeNode(strstr, doc);
    }
    catch (Poco::XML::XMLException &e) {
        LOG(ERROR) << "Failed to serialize XML to string due to: " << e.what();
    }
    catch (std::exception &e) {
        LOG(ERROR) << "Failed to serialize XML to string due to: " << e.what();
    }

    return strstr.str();
}

std::string toString(bool value) { return value ? "true" : "false"; }

XMLPtr<XMLElement> addElement(
    XMLPtr<XMLDocument> parent, const std::string &name)
{
    XMLPtr<XMLElement> node{parent->createElement(name)};
    parent->appendChild(node);
    return node;
}

XMLPtr<XMLElement> addElement(
    XMLPtr<XMLElement> parent, const std::string &name)
{
    XMLPtr<XMLElement> node{parent->ownerDocument()->createElement(name)};
    parent->appendChild(node);
    return node;
}

void addTextElement(
    XMLPtr<XMLElement> parent, const std::string &name, const std::string &text)
{
    XMLPtr<XMLElement> node = parent->ownerDocument()->createElement(name);
    if (!text.empty())
        node->appendChild(parent->ownerDocument()->createTextNode(text));
    parent->appendChild(node);
}

template <>
std::string serialize<Aws::S3::Model::ListBucketsResult>(
    const Aws::S3::Model::ListBucketsResult &list)
{
    Poco::XML::AutoPtr<XMLDocument> doc{new XMLDocument};
    auto rootElement = addElement(doc, "ListAllMyBucketsResult");
    rootElement->setAttribute(
        "xmlns", "http://s3.amazonaws.com/doc/2006-03-01/");

    auto buckets = addElement(rootElement, "Buckets");

    serialize(buckets, list.GetBuckets());

    // TODO
    // addTextElement(rootElement, "IsTruncated", "false");

    return toString(doc);
}

template <>
std::string serialize<Aws::S3::Model::ListObjectsResult>(
    const Aws::S3::Model::ListObjectsResult &objects)
{
    XMLPtr<XMLDocument> doc = new XMLDocument;
    auto rootElement = addElement(doc, "ListBucketResult");
    rootElement->setAttribute(
        "xmlns", "http://s3.amazonaws.com/doc/2006-03-01/");

    if (!objects.GetCommonPrefixes().empty()) {
        serialize(rootElement, objects.GetCommonPrefixes());
    }

    serialize(rootElement, objects.GetContents());

    addTextElement(
        rootElement, "IsTruncated", toString(objects.GetIsTruncated()));
    addTextElement(rootElement, "Prefix", objects.GetPrefix());
    addTextElement(rootElement, "Name", objects.GetName());
    addTextElement(rootElement, "Marker", objects.GetMarker());
    addTextElement(rootElement, "NextMarker", objects.GetNextMarker());
    addTextElement(rootElement, "Delimiter", "");
    addTextElement(rootElement, "EncodingType", "url");
    addTextElement(
        rootElement, "MaxKeys", std::to_string(objects.GetMaxKeys()));

    return toString(doc);
}

template <>
std::string serialize<Aws::S3::Model::ListObjectsV2Result>(
    const Aws::S3::Model::ListObjectsV2Result &objects)
{
    XMLPtr<XMLDocument> doc = new XMLDocument;
    auto rootElement = addElement(doc, "ListBucketResult");
    rootElement->setAttribute(
        "xmlns", "http://s3.amazonaws.com/doc/2006-03-01/");

    if (!objects.GetCommonPrefixes().empty()) {
        serialize(rootElement, objects.GetCommonPrefixes());
    }

    serialize(rootElement, objects.GetContents());

    addTextElement(
        rootElement, "IsTruncated", toString(objects.GetIsTruncated()));
    addTextElement(rootElement, "Prefix", objects.GetPrefix());
    addTextElement(rootElement, "Name", objects.GetName());
    addTextElement(
        rootElement, "KeyCount", std::to_string(objects.GetKeyCount()));
    addTextElement(rootElement, "NextContinuationToken",
        objects.GetNextContinuationToken());
    addTextElement(
        rootElement, "ContinuationToken", objects.GetContinuationToken());
    addTextElement(rootElement, "Delimiter", "");
    addTextElement(rootElement, "EncodingType", "url");
    addTextElement(
        rootElement, "MaxKeys", std::to_string(objects.GetMaxKeys()));

    return toString(doc);
}

template <>
std::string serialize<Aws::S3::Model::CreateMultipartUploadResult>(
    const Aws::S3::Model::CreateMultipartUploadResult &result)
{
    XMLPtr<XMLDocument> doc = new XMLDocument;
    auto rootElement = addElement(doc, "InitiateMultipartUploadResult");
    rootElement->setAttribute(
        "xmlns", "http://s3.amazonaws.com/doc/2006-03-01/");

    addTextElement(rootElement, "Bucket", result.GetBucket());
    addTextElement(rootElement, "Key", result.GetKey());
    addTextElement(rootElement, "UploadId", result.GetUploadId());

    return toString(doc);
}

template <>
std::string serialize<Aws::S3::Model::CompleteMultipartUploadResult>(
    const Aws::S3::Model::CompleteMultipartUploadResult &result)
{
    XMLPtr<XMLDocument> doc = new XMLDocument;
    auto rootElement = addElement(doc, "CompleteMultipartUploadResult");
    rootElement->setAttribute(
        "xmlns", "http://s3.amazonaws.com/doc/2006-03-01/");

    addTextElement(rootElement, "Bucket", result.GetBucket());
    addTextElement(rootElement, "Key", result.GetKey());
    addTextElement(rootElement, "ETag", result.GetETag());

    return toString(doc);
}

template <>
std::string serialize<Aws::S3::Model::DeleteObjectsResult>(
    const Aws::S3::Model::DeleteObjectsResult &result)
{
    XMLPtr<XMLDocument> doc = new XMLDocument;
    auto rootElement = addElement(doc, "DeleteResult");
    rootElement->setAttribute(
        "xmlns", "http://s3.amazonaws.com/doc/2006-03-01/");

    for (const auto &deleted : result.GetDeleted()) {
        auto deletedElement = addElement(rootElement, "Deleted");
        addTextElement(deletedElement, "Key", deleted.GetKey());
    }

    for (const auto &error : result.GetErrors()) {
        auto errorElement = addElement(rootElement, "Error");
        addTextElement(errorElement, "Key", error.GetKey());
        addTextElement(errorElement, "Code", error.GetCode());
        addTextElement(errorElement, "Message", error.GetMessage());
    }

    return toString(doc);
}

template <>
std::string serialize<Aws::S3::Model::ListPartsResult>(
    const Aws::S3::Model::ListPartsResult &result)
{
    XMLPtr<XMLDocument> doc = new XMLDocument;
    auto rootElement = addElement(doc, "ListPartsResult");
    rootElement->setAttribute(
        "xmlns", "http://s3.amazonaws.com/doc/2006-03-01/");

    addTextElement(rootElement, "Bucket", result.GetBucket());
    addTextElement(rootElement, "Key", result.GetKey());
    addTextElement(rootElement, "UploadId", result.GetUploadId());
    addTextElement(rootElement, "StorageClass",
        Aws::S3::Model::StorageClassMapper::GetNameForStorageClass(
            result.GetStorageClass()));
    addTextElement(rootElement, "PartNumberMarker",
        std::to_string(result.GetPartNumberMarker()));
    addTextElement(rootElement, "NextPartNumberMarker",
        std::to_string(result.GetNextPartNumberMarker()));
    addTextElement(
        rootElement, "MaxParts", std::to_string(result.GetMaxParts()));
    addTextElement(
        rootElement, "IsTruncated", toString(result.GetIsTruncated()));

    serialize(rootElement, result.GetParts());

    return toString(doc);
}

template <>
void serialize(const XMLPtr<XMLElement> &parent,
    const Aws::Vector<Aws::S3::Model::Bucket> &buckets)
{
    for (const auto &bucket : buckets) {
        auto node = addElement(parent, "Bucket");

        addTextElement(node, "Name", bucket.GetName());
        addTextElement(node, "CreationDate",
            bucket.GetCreationDate().ToGmtString(
                Aws::Utils::DateFormat::RFC822));
    }
}

template <>
void serialize(const XMLPtr<XMLElement> &parent,
    const Aws::Vector<Aws::S3::Model::CommonPrefix> &prefixes)
{
    for (const auto &prefix : prefixes) {
        auto cp = addElement(parent, "CommonPrefixes");

        addTextElement(cp, "Prefix", prefix.GetPrefix());
    }
}

template <>
void serialize(const XMLPtr<XMLElement> &parent,
    const Aws::Vector<Aws::S3::Model::Object> &objects)
{
    for (const auto &object : objects) {
        auto node = addElement(parent, "Contents");

        addTextElement(node, "Key", object.GetKey());
        addTextElement(node, "Size", std::to_string(object.GetSize()));
        addTextElement(node, "ETag", fmt::format("{}", object.GetETag()));
        addTextElement(node, "LastModified",
            object.GetLastModified().ToGmtString(
                Aws::Utils::DateFormat::ISO_8601));
        addTextElement(node, "StorageClass", "STANDARD");
    }
}

template <>
void serialize(const XMLPtr<XMLElement> &parent,
    const Aws::Vector<Aws::S3::Model::Part> &parts)
{
    for (const auto &part : parts) {
        auto node = addElement(parent, "Part");

        addTextElement(
            node, "PartNumber", std::to_string(part.GetPartNumber()));
        addTextElement(node, "LastModified",
            part.GetLastModified().ToGmtString(Aws::Utils::DateFormat::RFC822));
        addTextElement(node, "ETag", part.GetETag());
        addTextElement(node, "Size", std::to_string(part.GetSize()));
    }
}

} // namespace s3
} // namespace one
