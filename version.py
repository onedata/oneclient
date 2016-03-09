import re
import sys
from subprocess import check_output, CalledProcessError

major, minor, patch, tweak, fallback = 0, 0, 0, '', 'unknown version'
try:
    version = check_output(['git', 'describe', '--tags', '--always']).strip()
    m = re.match('(?:(\d+)\.(\d+)\.(\d+)(?:-([\w-]+))?|(\w+))', version)
    major, minor, patch, tweak, fallback = m.groups()
    major = major if major else 0
    minor = minor if minor else 0
    patch = patch if patch else 0
    tweak = tweak if tweak else ''
    fallback = 'git-{}'.format(fallback) if fallback else ''
except (OSError, CalledProcessError):
    pass

version_file = 'version.txt'
if len(sys.argv) == 2:
    version_file = '{}/{}'.format(sys.argv[1], version_file)

cmake = '''
set(oneclient_VERSION_MAJOR {0})
set(oneclient_VERSION_MINOR {1})
set(oneclient_VERSION_PATCH {2})
set(oneclient_VERSION_TWEAK {3})
set(oneclient_VERSION_FALLBACK {4})
'''.format(major, minor, patch, tweak, fallback)

with open(version_file, 'w') as f:
    f.write(cmake)
