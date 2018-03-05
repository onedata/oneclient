# coding=utf-8
"""Author: Lukasz Opiola
Copyright (C) 2018 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Utility functions for creating web certs singed by a test CA.
"""

import tempfile
import shutil
import os
import binascii
import subprocess


def _cacert():
    return '''-----BEGIN RSA PRIVATE KEY-----
MIIEogIBAAKCAQEAsdYI47L2z+PnDbwclHBRbv/zjpvJ1irz/27tiVk2QEXt9DDy
Rv+HZo8YKSREI9e/kalJbIWHYVmvT3t/LUkJleTHxJbn0tkhCD24VYl91f5t8Ik1
FKhd0d23fIY1SCJ5TttaFKCJ65Yw3STBXTTCXWAp7ByDWU34ZJO3Cql0KKQ8AnLO
UrXllXmSM/RkZLanvEVJZ1nKOWIDDg2UC99SP+RBUNfjjRqTV8padWFfYlIih2xy
Iw+Yz6/pa2AFae9sgcwV36rSYJMFdS7LUa7mwn2UC98dfzbsmbdtE8RNUSGpJzQY
DqI5+uvjK5lhI/mD3m5FVyQpSnDLSzM7EKpWwQIDAQABAoIBAE6f0ZhSNHrfSPUy
mPQr4GUR9m2zhP4SSYRCV/Tko26xPC49QbCQIx393/G4ngO2zHSrAtJfCubIjxC2
ChlMdFd4KbZJ/b6jzs3k882llyP4dETndLdoZOp3ezsr3vzAeR+bLW13OOMWqQ3s
xSzfOZus+3vvc0cViN92y3OETd31WbIx/R/BgE1YQt7iPLHR8BJhETKOXcqzk9g2
SXfc2ttKb1E93rLhL5kwei3KqFaTpb67n8KaNpAcuCPiy26d3WnmUmc6T2xD9ttl
g5Olg88elCqVpI8/BgPRGQMBdKDNRr3eV5xnXqol/q/LFcvkPpGGu+iKxm6xy1U2
U9CE+m0CgYEA4hX7bFjfk6y9GUTV0nT8OdkECv6IMmcHTnn5dWmbvWte5yGUCsvr
lw6FSiV8hmxOJk/27azj3tU8HdvgITF4hVbF14ZFlmLIn5w9DZtPWXAcvaKzGZwR
hLnZQZK0rW8IEOnknQdCoplu+hOH6+M1roKLnr+yc1mPQmwyNw1WhLMCgYEAyV26
Qyt6BuQKLRUi5y2Pa6HGpDQBd8sh+D/FnOV6tyvatCyNDABCUk+u99GLC5x0B8mS
hyL80+gia7O7usDZ0lh3Vx5F1k1LkUKsAUqcBmTQXJ25tD0psENBCZE1KONvWCgP
ohdrFdfkONVmV2hqwLsSumOKSBj5H9jUxw9u+LsCgYB9qCwSnx68jDCZv8l77GtX
l4eIuMloAh1sf5ynpmfaErgvjFkk2wv47Cgm+sjISZ/x6VXb6dDIxAliqxdaO425
xm21iTpaCFNrasIAMwcaNZazy57xp+2QsF7Q9EIZdvGrvOPEZwmhJ9gng/1ynNj0
Qjhppi//rpJSH6KVOIOSwwKBgEliixsnoRUZzkuXxFyT0gzbrFTzTwWlVf/u3cnM
J+jRZqb7BXw2K/VrbMyL5SyaG/8qiugM7C8eDk1J90ScO8XYz8VEFxd+m+eYcK8X
zbzWyMi4ApGZKLRADle6P7FjEZUDJI9iEXiocVf1DZMVTrJmevDKjf6wezoL/598
FGk5AoGAdbefzsKzR0p1EDiFHdxns9grMSAbtypqBTQklSWombI1s6+AoeuQxbhd
F7uFZl/JPEQDykggmka78N1VRGZUHdYkQlgZfgXtEjFD6Fh8y7o0UQ2v0BzzebBf
gvIXJVCEPwfEjd2szdBnOMitpolvyFkqh6FhS5VzVvkm83Pfpn8=
-----END RSA PRIVATE KEY-----
-----BEGIN CERTIFICATE-----
MIIFBjCCA+6gAwIBAgIJAMffGq9X10myMA0GCSqGSIb3DQEBCwUAMIGyMQswCQYD
VQQGEwJQTDEfMB0GA1UECBMWT25lRGF0YVRlc3RXZWJTZXJ2ZXJDQTEfMB0GA1UE
BxMWT25lRGF0YVRlc3RXZWJTZXJ2ZXJDQTEfMB0GA1UEChMWT25lRGF0YVRlc3RX
ZWJTZXJ2ZXJDQTEfMB0GA1UECxMWT25lRGF0YVRlc3RXZWJTZXJ2ZXJDQTEfMB0G
A1UEAxMWT25lRGF0YVRlc3RXZWJTZXJ2ZXJDQTAeFw0xNzExMTAwOTE1NTZaFw0z
NzExMTAwOTE1NTZaMIGyMQswCQYDVQQGEwJQTDEfMB0GA1UECBMWT25lRGF0YVRl
c3RXZWJTZXJ2ZXJDQTEfMB0GA1UEBxMWT25lRGF0YVRlc3RXZWJTZXJ2ZXJDQTEf
MB0GA1UEChMWT25lRGF0YVRlc3RXZWJTZXJ2ZXJDQTEfMB0GA1UECxMWT25lRGF0
YVRlc3RXZWJTZXJ2ZXJDQTEfMB0GA1UEAxMWT25lRGF0YVRlc3RXZWJTZXJ2ZXJD
QTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBALHWCOOy9s/j5w28HJRw
UW7/846bydYq8/9u7YlZNkBF7fQw8kb/h2aPGCkkRCPXv5GpSWyFh2FZr097fy1J
CZXkx8SW59LZIQg9uFWJfdX+bfCJNRSoXdHdt3yGNUgieU7bWhSgieuWMN0kwV00
wl1gKewcg1lN+GSTtwqpdCikPAJyzlK15ZV5kjP0ZGS2p7xFSWdZyjliAw4NlAvf
Uj/kQVDX440ak1fKWnVhX2JSIodsciMPmM+v6WtgBWnvbIHMFd+q0mCTBXUuy1Gu
5sJ9lAvfHX827Jm3bRPETVEhqSc0GA6iOfrr4yuZYSP5g95uRVckKUpwy0szOxCq
VsECAwEAAaOCARswggEXMB0GA1UdDgQWBBSZgVC9+yFATKN3WUoX144hmhU7ATCB
5wYDVR0jBIHfMIHcgBSZgVC9+yFATKN3WUoX144hmhU7AaGBuKSBtTCBsjELMAkG
A1UEBhMCUEwxHzAdBgNVBAgTFk9uZURhdGFUZXN0V2ViU2VydmVyQ0ExHzAdBgNV
BAcTFk9uZURhdGFUZXN0V2ViU2VydmVyQ0ExHzAdBgNVBAoTFk9uZURhdGFUZXN0
V2ViU2VydmVyQ0ExHzAdBgNVBAsTFk9uZURhdGFUZXN0V2ViU2VydmVyQ0ExHzAd
BgNVBAMTFk9uZURhdGFUZXN0V2ViU2VydmVyQ0GCCQDH3xqvV9dJsjAMBgNVHRME
BTADAQH/MA0GCSqGSIb3DQEBCwUAA4IBAQA5XA78nHVx+ogFiXc6+WQMXIoSzBOu
uLgG7ZKfgpzRnNoU+HL/Z89Hp9uvTOuTdio+3Z0klR7QTSaQoeiM7VZ96YBsS7XK
6Idt9waJOeECpe60IPjo7yXKGyjsaWR38wAAcP8p4UuFIEH3IRAyz6p2OjC3lBUP
EHV39Rfkt4zTp3J+tQff9W6rq6QWyYXkpymmnBivZ6kGhVnxucseM7Jwr2KIfOCG
QPPy31sUoEtn/96jmRksknTjnTb0QcPs5xu5XRnLP6bMzFMxtABeGQ7CzDM0sA0O
9c496FWw/KsejTmZ8DqO3u/DIaQZ4zfwRvAo7i3L8oGyab9qC1XWbdwO
-----END CERTIFICATE-----'''


def write_ca_cert(path):
    write_file(path, _cacert())


def create_key(path):
    cmd(['openssl', 'genrsa', '-out', path, '2048'])


def create_csr(key_path, output_path, common_name):
    cmd(['openssl', 'req', '-new', '-key', key_path, '-out', output_path,
         '-subj', '/C=PL/L=OneDataTest/O=OneDataTest/CN=' + common_name])


def generate_webcert(hostname):
    temp_dir, config_file = create_temp_ca_dir(hostname)
    key_path = os.path.join(temp_dir, 'key.pem')
    csr_path = os.path.join(temp_dir, 'csr.pem')
    cert_path = os.path.join(temp_dir, 'cert.pem')
    ca_cert_path = os.path.join(temp_dir, 'ca.pem')
    write_ca_cert(ca_cert_path)
    create_key(key_path)
    create_csr(key_path, csr_path, hostname)

    cmd([
        'openssl', 'ca', '-batch',
        '-config', config_file,
        '-extensions', 'server_cert',
        '-days', '3650', '-notext',
        '-keyfile', ca_cert_path,
        '-cert', ca_cert_path,
        '-in', csr_path,
        '-out', cert_path
    ])

    res = read_file(key_path), read_file(cert_path), read_file(ca_cert_path)
    shutil.rmtree(temp_dir)
    return res


def create_temp_ca_dir(hostname):
    temp_dir = tempfile.mkdtemp()
    config_file = os.path.join(temp_dir, "openssl.cfg")
    index_file = os.path.join(temp_dir, "index.txt")
    serial_file = os.path.join(temp_dir, "serial")
    random_serial = binascii.b2a_hex(os.urandom(6))

    write_file(config_file, openssl_cnf(temp_dir, hostname))
    write_file(index_file, '')
    write_file(serial_file, random_serial)
    return temp_dir, config_file


def openssl_cnf(home, hostname):
    return '''
HOME                   = {home}
default_ca             = ca

[ ca ]
dir                    = $HOME
certs                  = $dir
crl_dir                = $dir
database               = $dir/index.txt
new_certs_dir          = $dir
serial                 = $dir/serial
crl                    = $dir/crl.pem
RANDFILE               = $dir/.rand
x509_extensions        = server_cert
default_days           = 3650
default_crl_days       = 30
default_md             = sha256
preserve               = no
policy                 = policy_anything

[ policy_anything ]
countryName            = optional
stateOrProvinceName    = optional
localityName           = optional
organizationName       = optional
organizationalUnitName = optional
commonName             = supplied
emailAddress           = optional

[ server_cert ]
basicConstraints       = CA:FALSE
nsCertType             = server
nsComment              = 'OpenSSL Generated Server Certificate'
subjectKeyIdentifier   = hash
authorityKeyIdentifier = keyid,issuer:always
extendedKeyUsage       = serverAuth
keyUsage               = digitalSignature, keyEncipherment
subjectAltName         = @alt_names

[ alt_names ]
DNS.1                  = {hostname}
'''.format(home=home, hostname=hostname)


def read_file(path):
    with open(path, 'r') as f:
        return f.read()


def write_file(path, content):
    with open(path, 'w') as f:
        f.write(content)


def cmd(tokens):
    child = subprocess.Popen(tokens, stdout=open(os.devnull, 'w'),
                             stderr=subprocess.STDOUT)
    child.communicate()
