/**
 * @file status.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/status.h"

#include "messages.pb.h"

#include <boost/bimap.hpp>
#include <boost/optional/optional_io.hpp>

#include <cassert>
#include <sstream>
#include <system_error>
#include <vector>

namespace {

using Translation = boost::bimap<one::clproto::Status::Code, std::errc>;

Translation createTranslation()
{
    using namespace one::clproto;

    const std::vector<Translation::value_type> pairs{
        {Status_Code_ok, static_cast<std::errc>(0)},
        {Status_Code_eafnosupport, std::errc::address_family_not_supported},
        {Status_Code_eaddrinuse, std::errc::address_in_use},
        {Status_Code_eaddrnotavail, std::errc::address_not_available},
        {Status_Code_eisconn, std::errc::already_connected},
        {Status_Code_e2big, std::errc::argument_list_too_long},
        {Status_Code_edom, std::errc::argument_out_of_domain},
        {Status_Code_efault, std::errc::bad_address},
        {Status_Code_ebadf, std::errc::bad_file_descriptor},
        {Status_Code_ebadmsg, std::errc::bad_message},
        {Status_Code_epipe, std::errc::broken_pipe},
        {Status_Code_econnaborted, std::errc::connection_aborted},
        {Status_Code_ealready, std::errc::connection_already_in_progress},
        {Status_Code_econnrefused, std::errc::connection_refused},
        {Status_Code_econnreset, std::errc::connection_reset},
        {Status_Code_exdev, std::errc::cross_device_link},
        {Status_Code_edestaddrreq, std::errc::destination_address_required},
        {Status_Code_ebusy, std::errc::device_or_resource_busy},
        {Status_Code_enotempty, std::errc::directory_not_empty},
        {Status_Code_enoexec, std::errc::executable_format_error},
        {Status_Code_eexist, std::errc::file_exists},
        {Status_Code_efbig, std::errc::file_too_large},
        {Status_Code_enametoolong, std::errc::filename_too_long},
        {Status_Code_enosys, std::errc::function_not_supported},
        {Status_Code_ehostunreach, std::errc::host_unreachable},
        {Status_Code_eidrm, std::errc::identifier_removed},
        {Status_Code_eilseq, std::errc::illegal_byte_sequence},
        {Status_Code_enotty, std::errc::inappropriate_io_control_operation},
        {Status_Code_eintr, std::errc::interrupted},
        {Status_Code_einval, std::errc::invalid_argument},
        {Status_Code_espipe, std::errc::invalid_seek},
        {Status_Code_eio, std::errc::io_error},
        {Status_Code_eisdir, std::errc::is_a_directory},
        {Status_Code_emsgsize, std::errc::message_size},
        {Status_Code_enetdown, std::errc::network_down},
        {Status_Code_enetreset, std::errc::network_reset},
        {Status_Code_enetunreach, std::errc::network_unreachable},
        {Status_Code_enobufs, std::errc::no_buffer_space},
        {Status_Code_echild, std::errc::no_child_process},
        {Status_Code_enolink, std::errc::no_link},
        {Status_Code_enolck, std::errc::no_lock_available},
        {Status_Code_enodata, std::errc::no_message_available},
        {Status_Code_enomsg, std::errc::no_message},
        {Status_Code_enoprotoopt, std::errc::no_protocol_option},
        {Status_Code_enospc, std::errc::no_space_on_device},
        {Status_Code_enosr, std::errc::no_stream_resources},
        {Status_Code_enxio, std::errc::no_such_device_or_address},
        {Status_Code_enodev, std::errc::no_such_device},
        {Status_Code_enoent, std::errc::no_such_file_or_directory},
        {Status_Code_esrch, std::errc::no_such_process},
        {Status_Code_enotdir, std::errc::not_a_directory},
        {Status_Code_enotsock, std::errc::not_a_socket},
        {Status_Code_enostr, std::errc::not_a_stream},
        {Status_Code_enotconn, std::errc::not_connected},
        {Status_Code_enomem, std::errc::not_enough_memory},
        {Status_Code_enotsup, std::errc::not_supported},
        {Status_Code_ecanceled, std::errc::operation_canceled},
        {Status_Code_einprogress, std::errc::operation_in_progress},
        {Status_Code_eperm, std::errc::operation_not_permitted},
        {Status_Code_eopnotsupp, std::errc::operation_not_supported},
        {Status_Code_ewouldblock, std::errc::operation_would_block},
        {Status_Code_eownerdead, std::errc::owner_dead},
        {Status_Code_eacces, std::errc::permission_denied},
        {Status_Code_eproto, std::errc::protocol_error},
        {Status_Code_eprotonosupport, std::errc::protocol_not_supported},
        {Status_Code_erofs, std::errc::read_only_file_system},
        {Status_Code_edeadlk, std::errc::resource_deadlock_would_occur},
        {Status_Code_eagain, std::errc::resource_unavailable_try_again},
        {Status_Code_erange, std::errc::result_out_of_range},
        {Status_Code_enotrecoverable, std::errc::state_not_recoverable},
        {Status_Code_etime, std::errc::stream_timeout},
        {Status_Code_etxtbsy, std::errc::text_file_busy},
        {Status_Code_etimedout, std::errc::timed_out},
        {Status_Code_enfile, std::errc::too_many_files_open_in_system},
        {Status_Code_emfile, std::errc::too_many_files_open},
        {Status_Code_emlink, std::errc::too_many_links},
        {Status_Code_eloop, std::errc::too_many_symbolic_link_levels},
        {Status_Code_eoverflow, std::errc::value_too_large},
        {Status_Code_eprototype, std::errc::wrong_protocol_type}};

    return {pairs.begin(), pairs.end()};
}

const Translation translation = createTranslation();
}

namespace one {
namespace messages {

Status::Status(std::error_code ec)
    : m_code{ec}
{
}

Status::Status(std::error_code ec, std::string desc)
    : m_code{ec}
    , m_description{std::move(desc)}
{
}

Status::Status(std::unique_ptr<ProtocolServerMessage> serverMessage)
    : Status{*serverMessage->mutable_status()}
{
}

Status::Status(clproto::Status &status)
{
    auto searchResult = translation.left.find(status.code());
    auto errc = searchResult == translation.left.end()
        ? std::errc::protocol_error
        : searchResult->second;

    m_code = std::make_error_code(errc);
    if (status.has_description())
        m_description = std::move(*status.mutable_description());
}

std::error_code Status::code() const { return m_code; }

void Status::throwOnError() const
{
    if (!m_code)
        return;

    if (m_description)
        throw std::system_error{m_code, m_description.get()};

    throw std::system_error{m_code};
}

const boost::optional<std::string> &Status::description() const
{
    return m_description;
}

std::string Status::toString() const
{
    std::stringstream stream;
    stream << "type: 'Status', code: " << m_code
           << ", description: " << m_description;
    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> Status::serializeAndDestroy()
{
    auto clientMsg = std::make_unique<ProtocolClientMessage>();
    auto statusMsg = clientMsg->mutable_status();

    auto searchResult =
        translation.right.find(static_cast<std::errc>(m_code.value()));

    assert(searchResult != translation.right.end());
    statusMsg->set_code(searchResult->second);

    if (m_description)
        statusMsg->mutable_description()->swap(m_description.get());

    return clientMsg;
}

} // namespace messages
} // namespace one
