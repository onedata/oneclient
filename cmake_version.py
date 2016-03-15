from __future__ import print_function
import re
from subprocess import check_output, CalledProcessError

major, minor, patch, tweak, fallback = 0, 0, 0, '', 'unknown version'
try:
    version = check_output(['git', 'describe', '--tags', '--always']).strip()
    m = re.match('(?:(\d+)\.(\d+)\.(\d+)(?:-([\w\-]+))?|(\w+))', version.decode())
    major, minor, patch, tweak, fallback = m.groups()
    major = major or 0
    minor = minor or 0
    patch = patch or 0
    tweak = tweak or ''
    fallback = 'git-{}'.format(fallback) if fallback else ''
except (OSError, CalledProcessError):
    pass

print('''
set(oneclient_VERSION_MAJOR {0})
set(oneclient_VERSION_MINOR {1})
set(oneclient_VERSION_PATCH {2})
set(oneclient_VERSION_TWEAK {3})
set(oneclient_VERSION_FALLBACK {4})
'''.format(major, minor, patch, tweak, fallback))
