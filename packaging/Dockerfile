FROM onedata/fedora-systemd:23
MAINTAINER Krzysztof Trzepla <krzysztof.trzepla@gmail.com>

# Build arguments
ARG RELEASE
ARG VERSION=""

# Get the image up to date
RUN dnf -y upgrade && \
    dnf clean all

WORKDIR /tmp

# Install oneclient package
RUN case ${RELEASE} in \
		production) \
			curl -o install.sh http://packages.onedata.org/install_oneclient.sh; \
                        sh install.sh ${VERSION}; \
			;; \
		*) \
			curl -o install.sh http://onedata-dev-packages.cloud.plgrid.pl/install_oneclient.sh; \
                        sh install.sh ${VERSION}; \
			;; \
	esac

# Install Samba and nfs servers
RUN dnf -y install samba rpcbind nfs-utils nfs-utils-lib
# Add Samba and nfs configs
ADD nfs_exports /etc/exports
ADD smb.conf /etc/samba/
# Add scritp to start Samba and nfs
ADD start_exports.sh  /root/start_exports.sh
ADD run.sh /root/run.sh

VOLUME /root/.local/share/oneclient /mnt/oneclient
ENV AUTHENTICATION=token
ENTRYPOINT ["/root/run.sh"]
CMD ["/mnt/oneclient"]
