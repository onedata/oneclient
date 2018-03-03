"""This module contains tests for extended attributes operations."""

__author__ = "Bartek Kryza"
__copyright__ = """(C) 2017 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

from test_common import *
from posix_test_types import *
import pytest

def generate_xattr_name():
    return "user."+random_str()

@pytest.mark.xattr_tests
def test_setxattr_should_set_extended_attribute(helper):
    file_id = random_str(32)
    xattr_name = generate_xattr_name()
    xattr_value = random_str()

    helper.write(file_id, '', 0)

    helper.setxattr(file_id, xattr_name, xattr_value, False, False)

    assert helper.getxattr(file_id, xattr_name) == xattr_value


@pytest.mark.xattr_tests
def test_setxattr_should_set_large_extended_attribute(helper):
    file_id = random_str(32)
    xattr_name = generate_xattr_name()
    xattr_value = 'A'*(3*1024)

    helper.write(file_id, '', 0)

    helper.setxattr(file_id, xattr_name, xattr_value, False, False)

    assert helper.getxattr(file_id, xattr_name) == xattr_value


@pytest.mark.xattr_tests
def test_setxattr_should_set_extended_attribute_with_empty_value(helper):
    file_id = random_str(32)
    xattr_name = generate_xattr_name()
    xattr_value = ""

    helper.write(file_id, '', 0)

    helper.setxattr(file_id, xattr_name, xattr_value, False, False)

    assert helper.getxattr(file_id, xattr_name) == xattr_value


@pytest.mark.xattr_tests
def test_setxattr_should_handle_create_replace_flags(helper):
    file_id = random_str(32)
    xattr_name = generate_xattr_name()
    xattr_value = random_str()
    xattr_value_replaced = random_str()

    helper.write(file_id, '', 0)

    # Create and Replace cannot be true at the same time
    with pytest.raises(RuntimeError) as excinfo:
        helper.setxattr(file_id, xattr_name, xattr_value, True, True)

    assert 'Invalid argument' in str(excinfo.value)

    # It should not be possible to replace non existent attribute
    with pytest.raises(RuntimeError) as excinfo:
        helper.setxattr(file_id, xattr_name, xattr_value, False, True)

    assert 'No data available' in str(excinfo.value)

    # It should be possible to replace existing attribute
    try:
        helper.setxattr(file_id, xattr_name, xattr_value, True, False)
        helper.setxattr(file_id, xattr_name, xattr_value_replaced, False, True)
    except:
        fail("Couldn't replace extended attribute: %s"%(xattr_name))

    assert helper.getxattr(file_id, xattr_name) == xattr_value_replaced

    # It should not be possible to explicitly create existing attribute
    with pytest.raises(RuntimeError) as excinfo:
        helper.setxattr(file_id, xattr_name, xattr_value, True, False)

    assert 'File exists' in str(excinfo.value)


@pytest.mark.xattr_tests
def test_getxattr_should_return_extended_attribute(helper):
    file_id = random_str(32)
    xattr_name = generate_xattr_name()
    xattr_value = random_str()

    helper.write(file_id, '', 0)

    with pytest.raises(RuntimeError) as excinfo:
        helper.getxattr(file_id, xattr_name)

    assert 'No data available' in str(excinfo.value)

    helper.setxattr(file_id, xattr_name, xattr_value, False, False)

    assert helper.getxattr(file_id, xattr_name) == xattr_value


@pytest.mark.xattr_tests
def test_removexattr_should_remove_extended_attribute(helper):
    file_id = random_str(32)
    xattr_name = generate_xattr_name()
    xattr_value = random_str()

    helper.write(file_id, '', 0)

    helper.setxattr(file_id, xattr_name, xattr_value, False, False)

    assert helper.getxattr(file_id, xattr_name) == xattr_value

    helper.removexattr(file_id, xattr_name)

    with pytest.raises(RuntimeError) as excinfo:
        helper.getxattr(file_id, xattr_name)

    assert 'No data available' in str(excinfo.value)


@pytest.mark.xattr_tests
def test_removexattr_should_remove_extended_attribute(helper):
    file_id = random_str(32)
    xattr_names = [generate_xattr_name() for i in xrange(10)]
    xattr_values = [random_str() for i in xrange(10)]
    xattrs = zip(xattr_names, xattr_values)

    helper.write(file_id, '', 0)

    for xattr in xattrs:
        helper.setxattr(file_id, xattr[0], xattr[1], False, False)

    assert len(helper.listxattr(file_id)) == len(xattrs)

    assert set(helper.listxattr(file_id)) == set(helper.listxattr(file_id))
