ARG BASE_IMAGE
FROM ${BASE_IMAGE}
MAINTAINER Krzysztof Trzepla <krzysztof.trzepla@gmail.com>

# Build arguments
ARG RELEASE_TYPE
ARG RELEASE="1909"
ARG VERSION=""
ARG DISTRIBUTION="bionic"
ARG FSONEDATAFS_VERSION=""
ARG ONECLIENT_PACKAGE="oneclient"
ARG HTTP_PROXY

# Enable Onedata devel package proxy cache
ENV http_proxy ${HTTP_PROXY}

# Get the image up to date and install utility tools
RUN apt-get -y update && \
    apt-get -y install bash-completion ca-certificates curl iputils-ping netcat \
                       man-db net-tools traceroute vim python-xattr parallel \
                       python-pip python3-pip gnupg2 xattr pv bsdmainutils && \
    apt-get clean

# Install fs.onedatafs dependencies
RUN pip install fs six setuptools xmlrunner pyyaml
RUN pip3 install fs six setuptools xmlrunner pyyaml

WORKDIR /tmp

# Temporarly disable dpkg path-exludes so that all Oneclient files are installed
# including docs and manpages
RUN mv /etc/dpkg/dpkg.cfg.d/excludes /tmp/excludes.bak

# Install oneclient package
RUN case ${RELEASE_TYPE} in \
        production) \
            curl -O http://packages.onedata.org/oneclient-${RELEASE}.sh; \
            ;; \
        *) \
            curl -O http://packages.devel.onedata.org/oneclient-${RELEASE}.sh; \
            ;; \
        esac && \
        sh oneclient-${RELEASE}.sh ${ONECLIENT_PACKAGE}=${VERSION}-1~${DISTRIBUTION} && \
        apt-get -y install python-onedatafs=${VERSION}-1~${DISTRIBUTION} \
                           python3-onedatafs=${VERSION}-1~${DISTRIBUTION} \
                           python-fs-plugin-onedatafs=${FSONEDATAFS_VERSION}-1~${DISTRIBUTION} \
                           python3-fs-plugin-onedatafs=${FSONEDATAFS_VERSION}-1~${DISTRIBUTION} \
                           sysbench

RUN mv /tmp/excludes.bak /etc/dpkg/dpkg.cfg.d/excludes

# Enable autocompletion
RUN echo "source /etc/bash_completion" >> /root/.bashrc

# Install Samba and NFS servers
RUN apt-get -y install samba rpcbind nfs-kernel-server && \
    apt-get clean

# Add Samba and NFS configs
ADD nfs_exports /etc/exports
ADD smb.conf /etc/samba/

# Add entrypoint script
ADD run.sh /root/run.sh

VOLUME /root/.local/share/oneclient /mnt/oneclient

ENV ONECLIENT_INSECURE=1

# Disable http_proxy for normal operation
ENV http_proxy ""

ENTRYPOINT ["/root/run.sh"]
CMD ["/mnt/oneclient"]
