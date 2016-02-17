FROM ubuntu:15.10
MAINTAINER Krzysztof Trzepla <krzysztof.trzepla@gmail.com>

# Build arguments
ARG RELEASE

# Get the image up to date
RUN apt-get update && \
    apt-get dist-upgrade -y && \
    apt-get clean

# Install required packages
RUN apt-get install -y curl

# Install oneclient package
RUN case ${RELEASE} in \
		production) \
			curl -sSL http://packages.onedata.org/install_oneclient.sh | sh \
			;; \
		*) \
			curl -sSL http://onedata-dev-packages.cloud.plgrid.pl/install_oneclient.sh | sh \
			;; \
	esac

# Set up the environment
RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

EXPOSE 53 5555 443 8443 80 8876 8877 9443

ENTRYPOINT ["oneclient"]
CMD ["--help"]
