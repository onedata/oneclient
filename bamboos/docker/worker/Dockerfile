FROM ubuntu:14.10
MAINTAINER Konrad Zemek <konrad.zemek@gmail.com>

# Get the image up to date
RUN apt-get update && \
    apt-get dist-upgrade -y && \
    apt-get clean all

# Install Erlang & Elixir build dependencies
RUN apt-get install -y curl git build-essential libncurses5-dev openssl \
                       libssl-dev fop xsltproc unixodbc-dev unzip && \
    apt-get clean all

# Build and install Erlang
ADD https://raw.githubusercontent.com/spawngrid/kerl/master/kerl kerl
RUN chmod +x kerl
RUN ./kerl build 17.4 17.4 && \
    ./kerl install 17.4 /opt/erlang && \
    ./kerl cleanup all
ENV PATH /opt/erlang/bin:$PATH

# Install runtime dependencies
ADD https://get.docker.com/builds/Linux/x86_64/docker-latest /usr/bin/docker
RUN chmod a+x /usr/bin/docker

# Set up the environment
ENV HOME /root
EXPOSE 53 5555 443 8443 80 8876 8877
