FROM onedata/fedora-systemd:23
MAINTAINER Krzysztof Trzepla <krzysztof.trzepla@gmail.com>

# Build arguments
ARG RELEASE
ARG VERSION=""

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

ENTRYPOINT ["oneclient"]
CMD ["--help"]
