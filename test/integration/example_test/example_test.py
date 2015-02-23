import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.realpath(__file__))))
from test_common import project_dir, appmock_dir, docker_dir


class TestExample:
    @classmethod
    def setup_class(cls):
        pass

    @classmethod
    def teardown_class(cls):
        pass

    def test_example(self):
        assert project_dir
        assert appmock_dir
        assert docker_dir
        assert 1 == 1
