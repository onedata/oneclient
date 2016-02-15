# coding=utf-8
"""
Authors: Lukasz Opiola
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Client library to contact appmock instances.
"""

import base64
import json
import time

import requests

# Appmock remote control port
appmock_rc_port = 9999

# These defines determine how often the appmock server will be requested
# to check for condition when waiting for something.
# Increment rate causes each next interval to be longer.
WAIT_STARTING_CHECK_INTERVAL = 250
WAIT_INTERVAL_INCREMENT_RATE = 1.3

requests.packages.urllib3.disable_warnings()


class AppmockClient(object):
    def __init__(self, ip):
        self.ip = ip

    def tcp_endpoint(self, port):
        return AppmockTCPEndpoint(self, port)

    def reset_rest_history(self):
        return reset_rest_history(self.ip)

    def reset_tcp_history(self):
        return reset_tcp_server_history(self.ip)


class AppmockTCPEndpoint(object):
    def __init__(self, client, port):
        self.client = client
        self.ip = client.ip
        self.port = port

    def specific_message_count(self, message_binary):
        return tcp_server_specific_message_count(self.ip, self.port,
                                                 message_binary)

    def all_messages_count(self):
        return tcp_server_all_messages_count(self.ip, self.port)

    def connection_count(self):
        return tcp_server_connection_count(self.ip, self.port)

    def history(self):
        return tcp_server_history(self.ip, self.port)

    def send(self, message_binary, msg_count=1):
        return tcp_server_send(self.ip, self.port, message_binary, msg_count)

    def wait_for_any_messages(self, msg_count=1, accept_more=False,
                              return_history=False, timeout_sec=20):
        return tcp_server_wait_for_any_messages(self.ip, self.port, msg_count,
                                                accept_more, return_history,
                                                timeout_sec)

    def wait_for_connections(self, number_of_connections=1, accept_more=False,
                             timeout_sec=20):
        return tcp_server_wait_for_connections(self.ip, self.port,
                                               number_of_connections,
                                               accept_more, timeout_sec)

    def wait_for_specific_messages(self, message_binary, msg_count=1,
                                   accept_more=False, return_history=False,
                                   timeout_sec=20):
        return tcp_server_wait_for_specific_messages(self.ip, self.port,
                                                     message_binary, msg_count,
                                                     accept_more,
                                                     return_history,
                                                     timeout_sec)


def _http_post(ip, port, path, use_ssl, data):
    """
    Helper function that perform a HTTP GET request
    Returns a tuple (Code, Headers, Body)
    """
    protocol = 'https' if use_ssl else 'http'
    response = requests.post(
        '{0}://{1}:{2}{3}'.format(protocol, ip, port, path),
        data, verify=False, timeout=10)
    return response.status_code, response.headers, response.text


def rest_endpoint_request_count(appmock_ip, endpoint_port, endpoint_path):
    """
    Returns how many times has given endpoint been requested.
    IMPORTANT: the endpoint_path must be literally the same as in app_desc
    module, for example: '/test1/[:binding]'
    """
    json_data = {
        'port': endpoint_port,
        'path': endpoint_path
    }
    _, _, body = _http_post(appmock_ip, appmock_rc_port,
                            '/rest_endpoint_request_count', True,
                            json.dumps(json_data))
    body = json.loads(body)
    if body['result'] == 'error':
        raise Exception(
            'rest_endpoint_request_count returned error: ' + body['reason'])
    return body['result']


def verify_rest_history(appmock_ip, expected_history):
    """
    Verifies if rest endpoints were requested in given order.
    Returns True or False.
    The expected_history is a list of tuples (port, path), for example:
    [(8080, '/test1/[:binding]'), (8080, '/test2')]
    """

    def create_endpoint_entry(_port, _path):
        entry = {
            'endpoint': {
                'path': _path,
                'port': _port
            }
        }
        return entry

    json_data = [create_endpoint_entry(port, path) for (port, path) in
                 expected_history]
    _, _, body = _http_post(appmock_ip, appmock_rc_port,
                            '/verify_rest_history', True,
                            json.dumps(json_data))
    body = json.loads(body)
    if body['result'] == 'error':
        raise Exception(
            'expected history does not match: ' + str(body['history']))
    return body['result']


def reset_rest_history(appmock_ip):
    """
    Performs a request to an appmock instance to reset
    all the history connected with ALL mocked rest endpoints.
    The reset will cause this instance
    to act the same as if it was restarted clean.
    """
    _, _, body = _http_post(appmock_ip, appmock_rc_port, '/reset_rest_history',
                            True, '')
    body = json.loads(body)
    if body['result'] == 'error':
        raise Exception(
            'reset_rest_history returned error: ' + body['reason'])
    return body['result']


def tcp_server_specific_message_count(appmock_ip, tcp_port, message_binary):
    """
    Returns number of messages exactly matching given message,
    that has been received by the TCP server mock.
    """
    encoded_message = base64.b64encode(message_binary)
    path = '/tcp_server_specific_message_count/{0}'.format(tcp_port)
    _, _, body = _http_post(appmock_ip, appmock_rc_port, path,
                            True, encoded_message)
    body = json.loads(body)
    if body['result'] == 'error':
        raise Exception(
            'tcp_server_specific_message_count returned error: ' +
            body['reason'])
    return body['result']


def tcp_server_wait_for_specific_messages(appmock_ip, tcp_port, message_binary,
                                          msg_count=1, accept_more=False,
                                          return_history=False, timeout_sec=20):
    """
    Returns when given number of specific messages
    has been received on given port, or after it timeouts.
    The accept_more flag makes the function succeed when
    there is the same or more messages than expected.
    The return_history flag causes the function
    to return full msg history upon success.
    """
    start_time = time.time()
    wait_for = WAIT_STARTING_CHECK_INTERVAL

    while True:
        result = tcp_server_specific_message_count(appmock_ip, tcp_port,
                                                   message_binary)
        if accept_more and result >= msg_count:
            break
        elif result == msg_count:
            break
        elif time.time() - start_time > timeout_sec:
            raise Exception(
                'tcp_server_wait_for_specific_messages returned error: timeout')
        else:
            time.sleep(wait_for / 1000.0)
            wait_for *= WAIT_INTERVAL_INCREMENT_RATE
    if return_history:
        return tcp_server_history(appmock_ip, tcp_port)


def tcp_server_all_messages_count(appmock_ip, tcp_port):
    """
    Returns number of all messages
    that has been received by the TCP server mock.
    """
    path = '/tcp_server_all_messages_count/{0}'.format(tcp_port)
    _, _, body = _http_post(appmock_ip, appmock_rc_port, path, True, '')
    body = json.loads(body)
    if body['result'] == 'error':
        raise Exception(
            'tcp_server_all_messages_count returned error: ' + body['reason'])
    return body['result']


def tcp_server_wait_for_any_messages(appmock_ip, tcp_port, msg_count=1,
                                     accept_more=False, return_history=False,
                                     timeout_sec=20):
    """
    Returns when given number of any messages has been received on given port,
    or after it timeouts.
    The accept_more flag makes the function succeed when
    there is the same or more messages than expected.
    The return_history flag causes the function to return
    full msg history upon success.
    """
    start_time = time.time()
    wait_for = WAIT_STARTING_CHECK_INTERVAL

    while True:
        result = tcp_server_all_messages_count(appmock_ip, tcp_port)
        if accept_more and result >= msg_count:
            break
        elif result == msg_count:
            break
        elif time.time() - start_time > timeout_sec:
            raise Exception(
                'tcp_server_wait_for_any_messages returned error: timeout')
        else:
            time.sleep(wait_for / 1000.0)
            # No incrementing wait time here because
            # this fun might be used for benchmarking.
    if return_history:
        return tcp_server_history(appmock_ip, tcp_port)


def tcp_server_send(appmock_ip, tcp_port, message_binary, msg_count=1):
    """
    Orders appmock to send given message to all
    connected clients, given amount of times.
    """
    encoded_message = base64.b64encode(message_binary)
    path = '/tcp_server_send/{0}/{1}'.format(tcp_port, msg_count)
    _, _, body = _http_post(appmock_ip, appmock_rc_port, path, True,
                            encoded_message)
    body = json.loads(body)
    if body['result'] == 'error':
        raise Exception('tcp_server_send returned error: ' + body['reason'])
    return body['result']


def tcp_server_history(appmock_ip, tcp_port):
    """
    Performs a request to an appmock instance to
    obtain full history of messages received on given endpoint.
    """
    _, _, body = _http_post(appmock_ip, appmock_rc_port,
                            '/tcp_server_history/{0}'.format(tcp_port),
                            True, '')
    body = json.loads(body)
    if body['result'] == 'error':
        raise Exception('tcp_server_send returned error: ' + body['reason'])
    for i in range(len(body['result'])):
        body['result'][i] = base64.b64decode(body['result'][i])
    return body['result']


def reset_tcp_server_history(appmock_ip):
    """
    Performs a request to an appmock instance to reset
    all the history connected with ALL mocked TCP endpoints.
    The reset will cause this instance to act
    the same as if it was restarted clean - e. g. counters will be reset.
    Existing connections WILL NOT BE DISTURBED.
    """
    _, _, body = _http_post(appmock_ip, appmock_rc_port,
                            '/reset_tcp_server_history', True, '')
    body = json.loads(body)
    if body['result'] == 'error':
        raise Exception(
            'reset_tcp_history returned error: ' + body['reason'])
    return body['result']


def tcp_server_connection_count(appmock_ip, tcp_port):
    """
    Performs a request to an appmock instance to check
    how many clients are connected to given endpoint (by port).
    """
    path = '/tcp_server_connection_count/{0}'.format(tcp_port)
    _, _, body = _http_post(appmock_ip, appmock_rc_port, path, True, '')
    body = json.loads(body)
    if body['result'] == 'error':
        raise Exception(
            'tcp_server_connection_count returned error: ' + body['reason'])
    return body['result']


def tcp_server_wait_for_connections(appmock_ip, tcp_port,
                                    number_of_connections=1, accept_more=False,
                                    timeout_sec=20):
    """
    Returns when given number of connections
    are established on given port, or after it timeouts.
    The accept_more flag makes the function succeed when
    there is the same or more connections than expected.
    """
    start_time = time.time()
    wait_for = WAIT_STARTING_CHECK_INTERVAL

    while True:
        result = tcp_server_connection_count(appmock_ip, tcp_port)
        if accept_more and result >= number_of_connections:
            return
        elif result == number_of_connections:
            return
        elif time.time() - start_time > timeout_sec:
            raise Exception(
                'tcp_server_connection_count returned error: timeout')
        else:
            time.sleep(wait_for / 1000.0)
            wait_for *= WAIT_INTERVAL_INCREMENT_RATE
