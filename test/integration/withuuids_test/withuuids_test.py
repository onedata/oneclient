__author__ = "Bartek Kryza"
__copyright__ = """(C) 2024 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

import os
import sys
from threading import Thread
from multiprocessing import Pool
import time
import math
import json
import pytest
import hashlib

script_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.dirname(script_dir))
import withuuids


@pytest.fixture(scope="module")
def http_mock_server():
    srv = withuuids.HTTPMockServer()
    srv.start()
    time.sleep(2)
    yield srv
    srv.stop()


@pytest.fixture(scope="function")
def wuuids(http_mock_server):
    wu = withuuids.WithUuidsProxy("127.0.0.1", "")
    yield wu


@pytest.fixture(scope="function")
def wuuids_whitelisted(http_mock_server):
    wu = withuuids.WithUuidsProxy("127.0.0.1",
                                  "--space Space1 --space Space2 --space Space3")
    yield wu


@pytest.fixture(scope="function")
def wuuids_spaceids(http_mock_server):
    wu = withuuids.WithUuidsProxy("127.0.0.1", "--show-space-ids")
    yield wu


def generate_infer_access_scope(space_names):
    def generate_hash(name):
        return hashlib.sha256(name.encode('utf-8')).hexdigest()

    current_time = int(time.time())
    valid_until = current_time + 3600  # for example, valid for 1 hour

    data_access_scope = {
        "readonly": False,
        "spaces": {},
        "providers": {
            "6c215505a969202341a74c8f2c394318ch47a3": {
                "version": "21.02.5",
                "online": True,
                "name": "Krakow",
                "domain": "krakow.onedata.example.com"
            },
            "36e032c90969f520b84bf55bbc4d35c6chd39b": {
                "version": "21.02.5",
                "online": True,
                "name": "Lisbon",
                "domain": "lisbon.onedata.example.com"
            }
        }
    }

    for name in space_names:
        space_hash = generate_hash(name)
        data_access_scope["spaces"][space_hash] = {
            "supports": {
                "6c215505a969202341a74c8f2c394318ch47a3": {
                    "readonly": False
                },
                "36e032c90969f520b84bf55bbc4d35c6chd39b": {
                    "readonly": False
                }
            },
            "name": name
        }

    result = {
        "validUntil": valid_until,
        "dataAccessScope": data_access_scope
    }

    return result #json.dumps(result, indent=2)


def rename_space(access_scope, old_name, new_name):
    for space_id in access_scope["dataAccessScope"]["spaces"]:
        if access_scope["dataAccessScope"]["spaces"][space_id]["name"] == old_name:
           access_scope["dataAccessScope"]["spaces"][space_id]["name"] = new_name

    return access_scope


def test_lookup_nonexisting_space(http_mock_server, wuuids):
    http_mock_server.set_response(
        "/api/v3/onezone/tokens/infer_access_token_scope",
        json.dumps(generate_infer_access_scope(["aaa", "bbb"])))

    with pytest.raises(RuntimeError) as excinfo:
        wuuids.lookup(1, "no_such_space")

    assert 'No such file or directory' in str(excinfo.value)


def test_lookup_space(http_mock_server, wuuids):
    http_mock_server.set_response(
        "/api/v3/onezone/tokens/infer_access_token_scope",
        json.dumps(generate_infer_access_scope(["Space1", "Space2"])))

    stat1 = wuuids.lookup(1, "Space1")
    stat2 = wuuids.lookup(1, "Space2")

    assert stat1.ino != stat2.ino


def test_getattr_space(http_mock_server, wuuids):
    http_mock_server.set_response(
        "/api/v3/onezone/tokens/infer_access_token_scope",
        json.dumps(generate_infer_access_scope(["Space1", "Space2"])))

    stat1 = wuuids.lookup(1, "Space1")
    stat2 = wuuids.lookup(1, "Space2")

    assert wuuids.getattr(stat1.ino).ino == stat1.ino
    assert wuuids.getattr(stat2.ino).ino == stat2.ino


def test_getattr_renamed_space(http_mock_server, wuuids):
    access_scope = generate_infer_access_scope(["Space1", "Space2"])
    http_mock_server.set_response(
        "/api/v3/onezone/tokens/infer_access_token_scope",
        json.dumps(access_scope))

    stat1 = wuuids.lookup(1, "Space1")
    stat2 = wuuids.lookup(1, "Space2")

    assert wuuids.getattr(stat1.ino).ino == stat1.ino
    assert wuuids.getattr(stat2.ino).ino == stat2.ino

    access_scope = rename_space(access_scope, "Space1", "SpaceOne")

    http_mock_server.set_response(
        "/api/v3/onezone/tokens/infer_access_token_scope",
        json.dumps(access_scope))

    retries = 20
    while retries > 0:
        spaces = wuuids.readdir(1, 100, 0)
        if set(spaces) == set([".", "..", "SpaceOne", "Space2"]):
            break
        else:
            time.sleep(1)
            retries = retries - 1
            continue

    statOne = wuuids.getattr(stat1.ino)
    stat2 = wuuids.lookup(1, "Space2")

    assert wuuids.getattr(stat1.ino).ino == statOne.ino
    assert wuuids.getattr(stat2.ino).ino == stat2.ino


def test_readdir(http_mock_server, wuuids):
    http_mock_server.set_response(
        "/api/v3/onezone/tokens/infer_access_token_scope",
        json.dumps(generate_infer_access_scope(["aaa", "bbb"])))

    spaces = wuuids.readdir(1, 100, 0)
    assert set(spaces) == set([".", "..", "aaa", "bbb"])


def test_readdir_large(http_mock_server, wuuids):
    space_list = [f"Space{i}" for i in range(1, 2345)]

    http_mock_server.set_response(
        "/api/v3/onezone/tokens/infer_access_token_scope",
        json.dumps(generate_infer_access_scope(space_list)))

    spaces = []

    off = 0
    max = 250
    spaces_chunk = wuuids.readdir(1, max, off)
    spaces.extend(spaces_chunk)
    while len(spaces_chunk) > 0:
        off += max
        spaces_chunk = wuuids.readdir(1, max, off)
        spaces.extend(spaces_chunk)

    space_list.append(".")
    space_list.append("..")
    assert len(spaces) == len(space_list)
    assert set(spaces) == set(space_list)


def test_readdir_whitelisted(http_mock_server, wuuids_whitelisted):
    space_list = [f"Space{i}" for i in range(1, 20)]

    http_mock_server.set_response(
        "/api/v3/onezone/tokens/infer_access_token_scope",
        json.dumps(generate_infer_access_scope(space_list)))

    spaces = wuuids_whitelisted.readdir(1, 100, 0)
    assert set(spaces) == set([".", "..", "Space1", "Space2", "Space3"])


def test_readdir_spaceids(http_mock_server, wuuids_spaceids):
    space_list = [f"Space{i}" for i in range(1, 3)]

    http_mock_server.set_response(
        "/api/v3/onezone/tokens/infer_access_token_scope",
        json.dumps(generate_infer_access_scope(space_list)))

    spaces = wuuids_spaceids.readdir(1, 100, 0)
    assert set(spaces) == set([".", "..",
                               "0ed3235a6618cddb12b3fd3e509dfcd7d183ee9ae80d1c82bfaddfd17baf36d0",
                               "02a2382143cde55d5e8cef23fd36846f861227a3b9a1dc8ce901e1ec63fc7e6e"])


def test_readdir_sees_new_spaces(http_mock_server, wuuids):
    space_list = [f"Space{i}" for i in range(1, 20)]

    http_mock_server.set_response(
        "/api/v3/onezone/tokens/infer_access_token_scope",
        json.dumps(generate_infer_access_scope(space_list)))

    spaces = wuuids.readdir(1, 100, 0)

    space_list.append(".")
    space_list.append("..")

    assert set(spaces) == set(space_list)

    space_list = [f"Space{i}" for i in range(1, 40)]

    http_mock_server.set_response(
        "/api/v3/onezone/tokens/infer_access_token_scope",
        json.dumps(generate_infer_access_scope(space_list)))

    space_list.append(".")
    space_list.append("..")

    retries = 20
    while retries > 0:
        spaces = wuuids.readdir(1, 100, 0)
        if set(spaces) == set(space_list):
            break
        else:
            time.sleep(1)
            retries = retries - 1
            continue

    assert set(spaces) == set(space_list)


def test_readdir_sees_renamed_spaces(http_mock_server, wuuids):
    space_list = [f"Space{i}" for i in range(1, 20)]

    access_scope = generate_infer_access_scope(space_list)
    http_mock_server.set_response(
        "/api/v3/onezone/tokens/infer_access_token_scope",
        json.dumps(access_scope))

    spaces = wuuids.readdir(1, 100, 0)

    space_list.append(".")
    space_list.append("..")

    assert set(spaces) == set(space_list)

    access_scope = rename_space(access_scope, "Space1", "SpaceOne")

    http_mock_server.set_response(
        "/api/v3/onezone/tokens/infer_access_token_scope",
        json.dumps(access_scope))

    space_list[0] = "SpaceOne"

    retries = 20
    while retries > 0:
        spaces = wuuids.readdir(1, 100, 0)
        if set(spaces) == set(space_list):
            break
        else:
            time.sleep(1)
            retries = retries - 1
            continue

    assert set(spaces) == set(space_list)
