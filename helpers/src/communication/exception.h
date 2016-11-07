/**
 * @file exception.h
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_EXCEPTION_H
#define HELPERS_COMMUNICATION_EXCEPTION_H

#include <stdexcept>

namespace one {
namespace communication {

/**
 * A base class for communication exceptions.
 */
class Exception : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

/**
 * An @c Exception specialization for errors occuring while sending messages.
 */
class SendError : public Exception {
public:
    using Exception::Exception;
};

/**
 * An @c Exception specialization for errors occuring while receiving messages.
 */
class ReceiveError : public Exception {
public:
    using Exception::Exception;
};

/**
 * An @c one::communication::ReceiveError specialization representing an
 * exceeded timeout while waiting for message.
 */
class TimeoutExceeded : public ReceiveError {
    using ReceiveError::ReceiveError;
};

/**
 * An @c Exception specialization for errors occuring while connecting to the
 * server.
 */
class ConnectionError : public Exception {
public:
    using Exception::Exception;
};

/**
 * An @c one::communication::ConnectionError specialization for connection
 * errors occuring due to invalid server certificate.
 */
class InvalidServerCertificate : public ConnectionError {
public:
    using ConnectionError::ConnectionError;
};

} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_EXCEPTION_H
