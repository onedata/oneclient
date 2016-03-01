FROM onedata/fedora-systemd:23
MAINTAINER Krzysztof Trzepla <krzysztof.trzepla@gmail.com>

# Build arguments
ARG RELEASE
ARG VERSION=""

# Get the image up to date
RUN dnf -y upgrade && \
    dnf clean all

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

RUN dnf -y install samba
ADD start_samba.sh  /root/start_samba.sh
RUN chmod +x /root/start_samba.sh
RUN echo "/root/start_samba.sh" >> /root/.bashrc

ENTRYPOINT ["oneclient"]
CMD ["--help"]
