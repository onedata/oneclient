ARG BASE_IMAGE
FROM ${BASE_IMAGE}
MAINTAINER Krzysztof Trzepla <krzysztof.trzepla@gmail.com>

# Build arguments
ARG RELEASE_TYPE
ARG RELEASE="1902"
ARG VERSION=""
ARG DISTRIBUTION="bionic"
ARG FSONEDATAFS_VERSION=""
ARG ONECLIENT_PACKAGE="oneclient"

# Get the image up to date and install utility tools
RUN apt-get -y update && \
    apt-get -y install bash-completion ca-certificates curl iputils-ping netcat \
                       man-db net-tools traceroute vim python-xattr parallel \
                       python-pip python3-pip gnupg2 && \
    apt-get clean

# Install fs.onedatafs dependencies
RUN pip install fs six setuptools
RUN pip3 install fs six setuptools

WORKDIR /tmp

# Install oneclient package
RUN case ${RELEASE_TYPE} in \
        production) \
            curl -O http://get.onedata.org/oneclient-${RELEASE}.sh; \
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

ENTRYPOINT ["/root/run.sh"]
CMD ["/mnt/oneclient"]
