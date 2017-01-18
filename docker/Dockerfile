FROM ubuntu:16.04
MAINTAINER Krzysztof Trzepla <krzysztof.trzepla@gmail.com>

# Build arguments
ARG RELEASE
ARG VERSION=""

# Get the image up to date and install utility tools
RUN apt-get -y update && \
    apt-get -y upgrade && \
    apt-get -y install bash-completion ca-certificates curl iputils-ping \
                       man-db net-tools traceroute vim && \
    apt-get clean

WORKDIR /tmp

# Install oneclient package
RUN case ${RELEASE} in \
        production) \
            curl -O http://get.onedata.org/oneclient.sh; \
            ;; \
        *) \
            curl -O http://onedata-dev-packages.cloud.plgrid.pl/oneclient.sh; \
            ;; \
        esac && \
        sh oneclient.sh oneclient=${VERSION}-1

# Enable autocompletion
RUN echo "source /etc/bash_completion" >> /root/.bashrc && \
    ln -s /var/lib/oneclient/oneclient.bash-completion /etc/bash_completion.d/oneclient

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
