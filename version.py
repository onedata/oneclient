import os
import sys
from subprocess import check_output, CalledProcessError, call

NULL = open(os.devnull, 'w')

major, minor, patch, tweak, fallback = 0, 0, 0, '', 'unknown version'

try:
    check_output(['which', 'git'])
    short_version = check_output(['git', 'describe', '--tags', '--abbrev=0', '--always']).strip()
    is_tag = (call(['git', 'show-ref', '--verify', 'refs/tags/{}'.format(short_version)], stdout=NULL) == 0)
    if is_tag:
        last_tag_hash = check_output(['git', 'rev-parse', short_version]).strip()
        current_hash = check_output(['git', 'rev-parse', 'HEAD']).strip()
        on_tag = (last_tag_hash == current_hash)
    else:
        on_tag = False
    version = check_output(['git', 'describe', '--tags', '--always']).strip()

    if is_tag:
        major, minor, suffix = version.split('.')
        if suffix.isdigit():
            patch, tweak = suffix, ''
        else:
            patch, tweak = suffix.split('-', 1)
        fallback = ''
    else:
        major, minor, patch, tweak = 0, 0, 0, ''
        fallback = 'git-{}'.format(version)

except CalledProcessError:
    major, minor, patch, tweak, fallback = 0, 0, 0, '', 'unknown version'
version_file = 'version.txt'
if len(sys.argv) == 2:
    version_file = '{}/{}'.format(sys.argv[1], version_file)

with open(version_file, 'w') as f:
    f.write("set( SAVED_VERSION_MAJOR {} )\n".format(major))
    f.write("set( SAVED_VERSION_MINOR {} )\n".format(minor))
    f.write("set( SAVED_VERSION_PATCH {} )\n".format(patch))
    f.write("set( SAVED_VERSION_TWEAK \"{}\" )\n".format(tweak))
    f.write("set( SAVED_VERSION_FALLBACK \"{}\" )\n".format(fallback))
