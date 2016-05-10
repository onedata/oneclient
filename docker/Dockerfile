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
			curl -o install.sh http://get.onedata.org/oneclient.sh; \
                        sh install.sh ${VERSION}; \
			;; \
		*) \
			curl -o install.sh http://onedata-dev-packages.cloud.plgrid.pl/oneclient.sh; \
                        sh install.sh ${VERSION}; \
			;; \
	esac

# Install Samba and nfs servers
# Install older Samba due to mounting error using OSX Finder
# in 3.4.8 version from repository
RUN dnf -y install samba-2:4.3.0-0.1.rc4.fc23.x86_64 rpcbind nfs-utils \
                   nfs-utils-lib && \
    dnf clean all
# Add Samba and nfs configs
ADD nfs_exports /etc/exports
ADD smb.conf /etc/samba/
# Add scritp to start Samba, nfs and oneclient
ADD run.sh /root/run.sh

VOLUME /root/.local/share/oneclient /mnt/oneclient
ENV AUTHENTICATION=token
ENTRYPOINT ["/root/run.sh"]
CMD ["/mnt/oneclient"]
