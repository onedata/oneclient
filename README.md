# oneclient

*oneclient* is a command line [Onedata](onedata.org) client. It provides a POSIX
interface to user's files in *onedata* system.

# User Guide

## Building

### Dependencies

An up-to-date list of *oneclient* build dependencies for Ubuntu and Fedora is
available in [control](pkg_config/debian/control) and
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
Usage: oneclient [options] mountpoint

A Onedata command line client.

General options:
  -h [ --help ]                         Show this help and exit.
  -V [ --version ]                      Show current Oneclient version and
                                        exit.
  -u [ --unmount ]                      Unmount Oneclient and exit.
  -c [ --config ] <path> (=/etc/oneclient.conf)
                                        Specify path to config file.
  -H [ --host ] <host>                  Specify the hostname of the Oneprovider
                                        instance to which the Oneclient should
                                        connect.
  -P [ --port ] <port> (=5555)          Specify the port to which the Oneclient
                                        should connect on the Oneprovider.
  -i [ --insecure ]                     Disable verification of server
                                        certificate, allows to connect to
                                        servers without valid certificate.
  -t [ --token ] <token>                Specify Onedata access token for
                                        authentication and authorization.
  -l [ --log-dir ] <path> (=/tmp/oneclient/0)
                                        Specify custom path for Oneclient logs.

FUSE options:
  -f [ --foreground ]         Foreground operation.
  -d [ --debug ]              Enable debug mode (implies -f).
  -s [ --single-thread ]      Single-threaded operation.
  -o [ --opt ] <mount_option> Pass mount arguments directly to FUSE.
```

### Configuration

Besides commandline configuration options, oneclient reads options from a global
configuration file located at `/usr/local/etc/oneclient.conf`
(`/etc/oneclient.conf` when installed from the package). Refer to the
[example configuration file](config/oneclient.conf) for details on the options.

#### Environment variables

Some options in the config file can be overridden using environment variables,
whose names are capitalized version of the config options. For the up-to-date
list of supported environment variables please refer to *oneclient*
[manpage](man/oneclient.1).

## Running oneclient docker image

Running dockerized *oneclient* is easy:

```
docker run -it --privileged onedata/oneclient:3.0.0-rc11
```

### Persisting the token

The application will ask for a token and run in the foreground. In order for
*oneclient* to remember your token, mount volume `/root/.local/share/oneclient`:

```
docker run -it --privileged -v ~/.oneclient_local:/root/.local/share/oneclient onedata/oneclient:3.0.0-rc11
```

You can also pass your token in `ONECLIENT_ACCESS_TOKEN` environment variable:

```
docker run -it --privileged -e ONECLIENT_ACCESS_TOKEN=$TOKEN onedata/oneclient:3.0.0-rc11
```

If *oneclient* knows the token (either by reading its config file or by reading
the environment variable), it can be run as a daemon container:

```
docker run -d --privileged -e ONECLIENT_ACCESS_TOKEN=$TOKEN onedata/oneclient:3.0.0-rc11
```

### Accessing your data

*oneclient* exposes NFS and SMB services for easy outside access to your mounted
spaces.

```
docker run -d --privileged -e ONECLIENT_ACCESS_TOKEN=$TOKEN onedata/oneclient:3.0.0-rc11

# Display container's IP address
docker inspect --format "{{ .NetworkSettings.IPAddress }}" $(docker ps -ql)
```

Now you can mount using *NFS* or *Samba* with:

```
nfs://<CONTAINER_IP_ADDR>/mnt/oneclient
smb://<CONTAINER_IP_ADDR>/onedata
```
