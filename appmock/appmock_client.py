import json
import requests
import time

# Appmock remote control port
appmock_rc_port = 9999

# These defines determine how often the appmock server will be requested to check for condition
# when waiting for something. Increment rate causes each next interval to be longer
WAIT_STARTING_CHECK_INTERVAL = 100
WAIT_INTERVAL_INCREMENT_RATE = 1.3


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
    Performs a request to an appmock instance to reset all the history connected with ALL mocked rest endpoints.
    The reset will cause this instance to act the same as if it was restarted clean.
    """
    _, _, body = _http_post(appmock_ip, appmock_rc_port, '/reset_rest_history', True, '')
    body = json.loads(body)
    if body['result'] == 'error':
        raise Exception(
            'reset_rest_history returned error: ' + body['reason'])
    return body['result']


def tcp_server_message_count(appmock_ip, tcp_port, message_binary):
    """
    Returns number of messages exactly matching given message,
    that has been received by the TCP server mock.
    """
    _, _, body = _http_post(appmock_ip, appmock_rc_port,
                            '/tcp_server_message_count/' + str(tcp_port),
                            True, message_binary)
    body = json.loads(body)
    if body['result'] == 'error':
        raise Exception(
            'tcp_server_message_count returned error: ' + body['reason'])
    return body['result']


def tcp_server_wait_for_messages(appmock_ip, tcp_port, data, number_of_messages, accept_more, timeout_sec):
    """
    Returns when given number of connections are established on given port, or after it timeouts.
    The accept_more flag makes the function succeed when there is the same or more messages than expected.
    """
    start_time = time.time()
    wait_for = WAIT_STARTING_CHECK_INTERVAL

    while True:
        result = tcp_server_message_count(appmock_ip, tcp_port, data)
        if accept_more and result >= number_of_messages:
            return
        elif result == number_of_messages:
            return
        elif time.time() - start_time > timeout_sec:
            raise Exception(
                'tcp_server_wait_for_messages returned error: timeout')
        else:
            time.sleep(wait_for / 1000.0)
            wait_for *= WAIT_INTERVAL_INCREMENT_RATE


def tcp_server_send(appmock_ip, tcp_port, message_binary):
    """
    Orders appmock to send given message to all connected clients."""
    _, _, body = _http_post(appmock_ip, appmock_rc_port,
                            '/tcp_server_send/' + str(tcp_port),
                            True, message_binary)
    body = json.loads(body)
    if body['result'] == 'error':
        raise Exception('tcp_server_send returned error: ' + body['reason'])
    return body['result']


def reset_tcp_server_history(appmock_ip):
    """
    Performs a request to an appmock instance to reset all the history connected with ALL mocked TCP endpoints.
    The reset will cause this instance to act the same as if it was restarted clean - e. g. counters will be reset.
    Existing connections WILL NOT BE DISTURBED.
    """
    _, _, body = _http_post(appmock_ip, appmock_rc_port, '/reset_tcp_server_history', True, '')
    body = json.loads(body)
    if body['result'] == 'error':
        raise Exception(
            'reset_tcp_history returned error: ' + body['reason'])
    return body['result']


def tcp_server_connection_count(appmock_ip, tcp_port):
    """
    Performs a request to an appmock instance to check how many clients are connected to given endpoint (by port).
    """
    _, _, body = _http_post(appmock_ip, appmock_rc_port, '/tcp_server_connection_count/' + str(tcp_port), True, '')
    body = json.loads(body)
    if body['result'] == 'error':
        raise Exception(
            'tcp_server_connection_count returned error: ' + body['reason'])
    return body['result']


def tcp_server_wait_for_connections(appmock_ip, tcp_port, number_of_connections, accept_more, timeout_sec):
    """
    Returns when given number of connections are established on given port, or after it timeouts.
    The accept_more flag makes the function succeed when there is the same or more connections than expected.
    """
    start_time = time.clock()
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
