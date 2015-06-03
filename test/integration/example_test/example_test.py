"""This file contains an example integration test code written in Python.
It's important that the name of this file ends with *_test.py so that pytest
can autodiscover it while looking for tests in the integration tests' top
directory. By convention, the basename of this file should be identical to the
name of test directory - in this case 'example_test'.
"""

import os
import sys

# This is a boilerplate code to add integration tests' top directory to Python
# path. The top directory contains test_common module, which among other things
# sets variables containing useful paths (project_dir, appmock_dir...) and
# extends the Python path with more directories.
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.realpath(__file__))))
from test_common import project_dir, appmock_dir, docker_dir
from performance import *


class TestExample:
    """This is an example test. It's important that its name starts with Test*.
    A single test file can contain a number of test classes.
    A class can have setup and teardown methods. Setup and teardown are also
    available on module level, method level, and for functions. For more
    information on the testing framework please see pytest documentation.
    """

    @classmethod
    def setup_class(cls):
        """An example setup method. It's a good place to perform actions ran
        once before a group of tests, like starting an appmock instance.
        """
        pass

    @classmethod
    def teardown_class(cls):
        """An example teardown method. Mainly, it should clean up after the
        same-level setup method.
        """
        pass

    @performance(skip=True)
    def test_example(self):
        """Methods whose name begin with test_* are automatically run by pytest.
        The primary tool used in these methods is 'assert', which checks for
        a condition, and if not true fails the test and prints the code that
        resulted in the failure.
        """
        assert project_dir
        assert appmock_dir
        assert docker_dir
        assert 1 == 1
