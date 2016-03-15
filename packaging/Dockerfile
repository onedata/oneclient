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

RUN dnf -y install samba rpcbind nfs-utils nfs-utils-lib
ADD start_exports.sh  /root/start_exports.sh
RUN chmod +x /root/start_exports.sh
RUN echo "/root/start_exports.sh" >> /root/.bashrc
ADD nfs_exports /etc/exports
ADD smb.conf /etc/samba/

VOLUME /root/.local/share/oneclient /mnt/oneclient
ENV AUTHENTICATION=token
ENTRYPOINT ["oneclient", "-f"]
CMD ["/mnt/oneclient"]
