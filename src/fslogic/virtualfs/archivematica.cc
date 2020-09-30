/**
 * @file archivematica.cc
 * @author Bartek Kryza
 * @copyright (C) 2020 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "archivematica.h"

#include "fslogic/fsLogic.h"
#include "messages/fuse/fileAttr.h"

#include <folly/json.h>

namespace one {
namespace client {
namespace virtualfs {
namespace archivematica {

ArchivematicaProcessingMCPFileHandle::ArchivematicaProcessingMCPFileHandle(
    folly::fbstring fileId, std::shared_ptr<StorageHelper> helper)
    : FileHandle{std::move(fileId), std::move(helper)}
{
}

folly::Future<folly::IOBufQueue> ArchivematicaProcessingMCPFileHandle::read(
    const off_t offset, const std::size_t size)
{
    if (offset + size > m_content.size())
        throw std::system_error(
            std::make_error_code(std::errc::result_out_of_range));

    auto res = folly::IOBufQueue{folly::IOBufQueue::cacheChainLength()};
    void *data = res.preallocate(size, size).first;
    memcpy(data, m_content.data() + offset, size);
    res.postallocate(size);

    return {std::move(res)};
}

folly::Future<std::size_t> ArchivematicaProcessingMCPFileHandle::write(
    const off_t /*offset*/, folly::IOBufQueue /*buf*/)
{
    throw std::system_error(
        std::make_error_code(std::errc::operation_not_permitted));
}

const Timeout &ArchivematicaProcessingMCPFileHandle::timeout()
{
    return m_timeout;
}

std::unique_ptr<FileLocation>
ArchivematicaProcessingMCPFileHandle::getLocation() const
{
    LOG_FCALL();

    auto loc = std::make_unique<FileLocation>();
    loc->putBlock(0, m_content.size(),
        FileBlock{"archivematica", m_fileId.toStdString()});
    loc->setUuid(m_fileId.toStdString());
    return loc;
}

folly::dynamic ArchivematicaProcessingMCPFileHandle::getProcessingMCPMetadata(
    const folly::fbstring &uuid)
{
    auto &fsLogic =
        std::dynamic_pointer_cast<ArchivematicaVirtualFsAdapter>(helper())
            ->fsLogic();

    auto options = folly::json::serialization_opts{};
    options.allow_nan_inf = true;
    options.double_fallback = false;
    options.javascript_safe = true;
    options.validate_utf8 = true;

    // Get processingMCP metadata from the parent directory or space
    folly::fbstring archivematicaMetadata;
    folly::dynamic jsonValue;

    try {
        archivematicaMetadata = fsLogic.getxattr(uuid, "onedata_json");
        jsonValue = folly::parseJson(archivematicaMetadata, options);
        if (jsonValue == nullptr || !jsonValue.isObject() ||
            jsonValue.count("__onedata") == 0U ||
            jsonValue["__onedata"].count("__archivematica") == 0U ||
            jsonValue["__onedata"]["__archivematica"].count("processingMCP") ==
                0U) {
            return {};
        }
        return jsonValue["__onedata"]["__archivematica"]["processingMCP"];
    }
    catch (...) {
        return {};
    }
}

void ArchivematicaProcessingMCPFileHandle::updateContent(
    std::shared_ptr<const FileAttr> attr)
{
    LOG_FCALL() << LOG_FARG(attr->uuid()) << LOG_FARG(attr->name());

    // Initialize the content with empty XML
    m_content = "<processingMCP><preconfiguredChoices>";
    m_content += "</preconfiguredChoices></processingMCP>";

    auto &fsLogic =
        std::dynamic_pointer_cast<ArchivematicaVirtualFsAdapter>(helper())
            ->fsLogic();
    if (!attr->parentUuid()) {
        LOG_DBG(2) << "File " << attr->name() << " has no parent - skipping...";
    }
    auto parentUuid = *attr->parentUuid();

    auto parentUuidIt = parentUuid;
    folly::dynamic pmcp = getProcessingMCPMetadata(parentUuidIt);
    while (pmcp == nullptr) {
        parentUuidIt =
            *(fsLogic.metadataCache().getAttr(parentUuidIt)->parentUuid());
        if (parentUuidIt == fsLogic.rootUuid())
            break;

        pmcp = getProcessingMCPMetadata(parentUuidIt);
    }
    if (pmcp == nullptr)
        return;

    try {
        if (pmcp.isObject()) {
            auto choicesMap = pmcp["preconfiguredChoices"];
            if (choicesMap == nullptr) {
                LOG(ERROR) << "Missing 'preconfiguredChoices' map for "
                           << attr->name();
                return;
            }

            auto choiceList = choicesMap["preconfiguredChoice"];
            if (choicesMap == nullptr) {
                LOG(ERROR) << "Missing 'preconfiguredChoice' list for "
                           << attr->name();
                return;
            }

            folly::fbstring content = "<processingMCP><preconfiguredChoices>";
            for (const auto &choice : choiceList) {
                auto appliesTo = choice["appliesTo"];
                auto goToChain = choice["goToChain"];
                if (appliesTo == nullptr) {
                    LOG(ERROR)
                        << "Missing 'appliesTo' value for " << attr->name();
                    return;
                }
                if (goToChain == nullptr) {
                    LOG(ERROR)
                        << "Missing 'goToChain' value for " << attr->name();
                    return;
                }
                content += "<preconfiguredChoice>";
                content +=
                    "<appliesTo>" + appliesTo.asString() + "</appliesTo>";
                content +=
                    "<goToChain>" + goToChain.asString() + "</goToChain>";
                content += "</preconfiguredChoice>";
            }
            content += "</preconfiguredChoices></processingMCP>";
            m_content = std::move(content);
        }

        LOG_DBG(2) << "Updated virtual file content for " << attr->name();
    }
    catch (std::exception &e) {
        LOG(ERROR) << "Parsing Archivematica Json metadata failed for "
                   << attr->name();
    }
}

ArchivematicaMetadataJSONFileHandle::ArchivematicaMetadataJSONFileHandle(
    folly::fbstring fileId, std::shared_ptr<StorageHelper> helper)
    : FileHandle{std::move(fileId), std::move(helper)}
{
}

std::unique_ptr<FileLocation>
ArchivematicaMetadataJSONFileHandle::getLocation() const
{
    LOG_FCALL();

    auto loc = std::make_unique<FileLocation>();
    loc->putBlock(0, m_content.size(),
        FileBlock{"archivematica", m_fileId.toStdString()});
    loc->setUuid(m_fileId.toStdString());
    return loc;
}

void ArchivematicaMetadataJSONFileHandle::appendMetadata(FsLogic &fsLogic,
    folly::fbstring parentUuid, folly::dynamic &objects, folly::fbstring prefix,
    bool topLevelDir)
{
    LOG_FCALL() << LOG_FARG(parentUuid) << LOG_FARG(prefix);

    const auto kMaxEntryNum{1024 * 1024UL};
    auto res = fsLogic.readdir(parentUuid, kMaxEntryNum, 0);

    auto parent = fsLogic.metadataCache().getAttr(parentUuid);
    auto parentName = parent->name();

    folly::fbvector<FileAttrPtr> files{};
    folly::fbvector<folly::fbstring> dirs{};

    for (const auto &entry : res) {
        if (entry.empty() || entry == "." || entry == "..")
            continue;

        auto attr = fsLogic.metadataCache().getAttr(parentUuid, entry);

        if (attr->isVirtual())
            continue;

        if (attr->type() == FileAttr::FileType::directory) {
            dirs.emplace_back(attr->uuid());
            continue;
        }

        files.emplace_back(std::move(attr));
    }

    auto topLevelPrefix{prefix};
    if (!topLevelDir) {
        topLevelPrefix += "/" + parentName;
    }

    for (const auto &dirUuid : dirs) {
        appendMetadata(fsLogic, dirUuid, objects, topLevelPrefix);
    }

    auto idx = objects.size();
    objects.resize(files.size() + objects.size());

    int skipped = 0;
    for (const auto &f : files) {
        folly::dynamic object = folly::dynamic::object;
        // Get files metadata and add them as properties of object
        auto extendedAttributes = fsLogic.listxattr(f->uuid());
        for (const auto &ex : extendedAttributes) {
            folly::StringPiece exView{ex};

            if (exView.startsWith("org.onedata.") ||
                (exView == "onedata_rdf") || (exView == "__onedata")) {
                continue;
            }

            if (exView == "onedata_json") {
                auto onedataJsonStr =
                    fsLogic.getxattr(f->uuid(), "onedata_json");
                try {
                    auto onedataJson = folly::parseJson(onedataJsonStr);
                    if (!onedataJson.isObject())
                        continue;
                    for (const auto &kv : onedataJson.items()) {
                        // Only regular top level attributes should be
                        // serialized for Archivematica
                        if (kv.second.isObject() || kv.second.isArray())
                            continue;

                        object[kv.first] = kv.second;
                    }
                }
                catch (...) {
                    LOG(ERROR) << "Failed parsing JSON for extended attribute "
                                  "onedata_json";
                }
            }
            else {
                auto options = folly::json::serialization_opts{};
                options.allow_nan_inf = true;
                options.double_fallback = false;
                options.javascript_safe = true;
                options.validate_utf8 = true;

                auto xattrValue = fsLogic.getxattr(f->uuid(), ex).toStdString();
                folly::dynamic jsonValue;
                try {
                    jsonValue = folly::parseJson(xattrValue, options);
                }
                catch (...) {
                }

                if (jsonValue == nullptr || jsonValue.isObject() ||
                    jsonValue.isArray()) {
                    object[ex.toStdString()] = xattrValue;
                }
                else {
                    object[ex.toStdString()] = jsonValue;
                }
            }
        }
        object["filename"] =
            topLevelPrefix.toStdString() + "/" + f->name().toStdString();

        // Only add entries for files, which have any metadata
        if (object.size() > 1)
            objects[idx++] = object;
        else
            skipped++;
    }
    objects.resize(objects.size() - skipped);
}

void ArchivematicaMetadataJSONFileHandle::updateContent(
    std::shared_ptr<const FileAttr> attr)
{
    auto &fsLogic =
        std::dynamic_pointer_cast<ArchivematicaVirtualFsAdapter>(helper())
            ->fsLogic();
    if (!attr->parentUuid()) {
        LOG_DBG(2) << "File " << attr->name() << " has no parent - skipping...";
    }
    auto parentUuid = *attr->parentUuid();
    auto topDirAttr = fsLogic.metadataCache().getAttr(parentUuid);
    auto topDirUuid = *topDirAttr->parentUuid();

    const std::string kObjectsPrefix{"objects"};
    folly::dynamic objects = folly::dynamic::array;

    appendMetadata(fsLogic, topDirUuid, objects, kObjectsPrefix, true);

    m_content = folly::toJson(objects);
}

folly::Future<folly::IOBufQueue> ArchivematicaMetadataJSONFileHandle::read(
    const off_t offset, const std::size_t size)
{
    if (offset + size > m_content.size())
        throw std::system_error(
            std::make_error_code(std::errc::result_out_of_range));

    auto res = folly::IOBufQueue{folly::IOBufQueue::cacheChainLength()};
    void *data = res.preallocate(size, size).first;
    memcpy(data, m_content.data() + offset, size);
    res.postallocate(size);

    return {std::move(res)};
}

folly::Future<std::size_t> ArchivematicaMetadataJSONFileHandle::write(
    const off_t /*offset*/, folly::IOBufQueue /*buf*/)
{
    throw std::system_error(
        std::make_error_code(std::errc::operation_not_permitted));
}

const Timeout &ArchivematicaMetadataJSONFileHandle::timeout()
{
    return m_timeout;
}

folly::fbstring ArchivematicaVirtualFsAdapter::name() const
{
    return "archivematica";
}

bool ArchivematicaVirtualFsAdapter::match(const folly::fbstring &direntry)
{
    folly::StringPiece direntryView{direntry};
    return direntryView.endsWith(".__onedata_archivematica");
}

folly::fbstring ArchivematicaVirtualFsAdapter::effectiveName(
    const folly::fbstring &direntry)
{
    folly::StringPiece d{direntry};
    d.removeSuffix(".__onedata_archivematica");
    return d.toFbstring();
}

folly::Future<FileHandlePtr> ArchivematicaVirtualFsAdapter::open(
    const folly::fbstring &fileId, const int /*flags*/,
    const Params & /*openParams*/)
{
    auto attr = fsLogic().metadataCache().getAttr(fileId);
    if (attr->name() == "processingMCP.xml") {
        auto handle = std::make_shared<ArchivematicaProcessingMCPFileHandle>(
            fileId, shared_from_this());

        try {
            handle->updateContent(attr);
        }
        catch (...) {
            throw std::system_error(
                std::make_error_code(std::errc::no_such_file_or_directory));
        }

        fsLogic().metadataCache().putLocation(handle->getLocation());
        fsLogic().metadataCache().updateSize(fileId, handle->size());

        return handle;
    }

    if (attr->name() == "metadata.json") {
        auto handle = std::make_shared<ArchivematicaMetadataJSONFileHandle>(
            fileId, shared_from_this());

        try {
            handle->updateContent(attr);
        }
        catch (...) {
            throw std::system_error(
                std::make_error_code(std::errc::no_such_file_or_directory));
        }

        fsLogic().metadataCache().putLocation(handle->getLocation());
        fsLogic().metadataCache().updateSize(fileId, handle->size());

        return handle;
    }

    throw std::system_error(
        std::make_error_code(std::errc::no_such_file_or_directory));
}

void ArchivematicaVirtualFsAdapter::readdir(
    const folly::fbvector<folly::fbstring> &entries,
    std::shared_ptr<const FileAttr> attr, MetadataCache &metadataCache)
{
    const auto &uuid = attr->uuid();
    if (attr->name() == "metadata") {
        if (std::find(entries.cbegin(), entries.cend(), "metadata.json") ==
            entries.cend()) {
            LOG_DBG(2) << "Adding metadata.json to metadatacache";
            auto a = std::make_shared<FileAttr>();
            a->setUuid(uuid + "-metadata-json");
            a->setParentUuid(uuid);
            a->size(1);
            a->setType(FileAttr::FileType::regular);
            a->setName("metadata.json");
            a->mode(S_IRUSR | S_IRGRP | S_IROTH);
            a->setVirtualFsAdapter(shared_from_this());
            metadataCache.putAttr(std::move(a));
        }
    }
    else {
        if (std::find(entries.cbegin(), entries.cend(), "processingMCP.xml") ==
            entries.cend()) {
            LOG_DBG(2) << "Adding processingMCP.xml to metadatacache";
            auto a = std::make_shared<FileAttr>();
            a->setUuid(uuid + "-processing-mcp");
            a->setParentUuid(uuid);
            a->size(1);
            a->setType(FileAttr::FileType::regular);
            a->setName("processingMCP.xml");
            a->mode(S_IRUSR | S_IRGRP | S_IROTH);
            a->setVirtualFsAdapter(shared_from_this());
            metadataCache.putAttr(std::move(a));
        }

        if (std::find(entries.cbegin(), entries.cend(), "metadata") ==
            entries.cend()) {
            auto a = std::make_shared<FileAttr>();
            a->setUuid(uuid + "-metadata");
            a->setParentUuid(uuid);
            a->setType(FileAttr::FileType::directory);
            a->setName("metadata");
            a->mode(S_IRUSR | S_IRGRP | S_IROTH | S_IXUSR | S_IXGRP | S_IXOTH);
            a->setVirtualFsAdapter(shared_from_this());
            metadataCache.putAttr(std::move(a));
        }
    }
}

bool ArchivematicaVirtualFsAdapter::fetchRemoteDirectoryContents(
    std::shared_ptr<const FileAttr> attr) const
{
    return attr->name() != "metadata";
}
} // namespace archivematica
} // namespace virtualfs
} // namespace client
} // namespace one
