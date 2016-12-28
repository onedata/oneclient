# oneclient

*oneclient* is a command line [Onedata](onedata.org) client. It provides a POSIX
interface to user's files in *onedata* system.

# User Guide

## Building

### Dependencies

To build *oneclient* you need the following packages installed on your system:

* boost-devel >= 1.58.0
* cmake >= 3.0.0
* fuse >= 2.7
* g++ >= 4.9.0 (or a recent version of Clang)
* go
* librados-dev
* libs3
* libsodium
* libtool-ltdl
* ninja
* openssl
* protobuf >= 2.6.0
* python
* tbb >= 4.3

An up-to-date list of packages for Ubuntu and Fedora is listed in
[control](pkg_config/debian/control) and
[oneclient.spec](pkg_config/oneclient.spec) files respectively.

## Compilation

```bash
git clone https://github.com/onedata/oneclient.git
cd oneclient
make release # or debug
```

The compiled binary `oneclient` will be created on path `release/oneclient` (or
`debug/oneclient`).

## Usage

```
release/oneclient [options] mountpoint

General options:
  -h [ --help ]            print help
  -V [ --version ]         print version
  --config arg             path to user config file
  --authentication arg     authentication type to use for connection with a
                           Provider. Accepted values are 'token' and
                           'certificate'.
  -d [ --debug ]           enable debug output (implies -f)
  --debug_gsi              enable GSI debug output
  --no_check_certificate   disable remote certificate validation
  --proxyio                force ProxyIO

FUSE options:
  -o opt,...               mount options
  -f                       foreground operation
  -s                       disable multi-threaded operation
```

### Configuration

Besides commandline configuration options, oneclient reads options from a global
configuration file located at `/usr/local/oneclient.conf` (`/etc/oneclient.conf`
when installed from the package). Refer to the
[example configuration file](config/oneclient.conf.default) for details on the
options.

#### Restricted and overridable options

Some options - marked as `[Restricted]` in the example configuration - can only
be set in the global configuration file. All other options can be overriden in
user config file (passed through the `--config` commandline options) or through
environment variable. For example, `PROVIDER_HOSTNAME` environment variable
overrides `provider_hostname` configuration option. Overriding with environment
variables can be disabled with `enable_env_option_override=false`.

#### Passing user token

When `--authentication` is set to `token`, *oneclient* reads user token from
standard input on first run. The token can also be passed to oneclient in
`ONECLIENT_AUTHORIZATION_TOKEN` environment variable.

## Running oneclient docker image

Running dockerized *oneclient* is easy:

```
docker run -ti --privileged onedata/oneclient:VFS-1951
```

### Persisting the token

The application will ask for a token and run in the foreground. In order for
*oneclient* to remember your token, mount volume `/root/.local/share/oneclient`:

```
docker run -ti --privileged -v ~/.oneclient_local:/root/.local/share/oneclient onedata/oneclient:VFS-1951
```

You can also pass your token in `ONECLIENT_AUTHORIZATION_TOKEN` environment
variable:

```
docker run -ti --privileged -e ONECLIENT_AUTHORIZATION_TOKEN=$TOKEN onedata/oneclient:VFS-1951
```

If *oneclient* knows the token (either by reading its config file or by reading
the environment variable), it can be run as a daemon container:

```
docker run -d --privileged -e ONECLIENT_AUTHORIZATION_TOKEN=$TOKEN onedata/oneclient:VFS-1951
```

### Accessing your data

*oneclient* exposes NFS and SMB services for easy outside access to your mounted
spaces.

```
docker run -d --privileged -e ONECLIENT_AUTHORIZATION_TOKEN=$TOKEN onedata/oneclient:VFS-1951

# Display container's IP address
docker inspect --format "{{ .NetworkSettings.IPAddress }}" $(docker ps -ql)
```

Now you can mount using *NFS* or *Samba* with:

```
nfs://<CONTAINER_IP_ADDR>/mnt/oneclient
smb://<CONTAINER_IP_ADDR>/onedata
```
