FROM ubuntu:14.10
MAINTAINER Konrad Zemek <konrad.zemek@gmail.com>

# Add custom package sources
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys D73BB29D 3A6CFFB3
RUN echo 'deb http://ppa.launchpad.net/lengau/protobuf/ubuntu utopic main' > /etc/apt/sources.list.d/protobuf.list
RUN echo 'deb http://ppa.launchpad.net/kzemek/intel-tbb/ubuntu utopic main' > /etc/apt/sources.list.d/intel-tbb.list
ADD http://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb /root/erlang-solutions_1.0_all.deb
RUN dpkg -i /root/erlang-solutions_1.0_all.deb

# Get the image up to date
RUN apt-get update && \
    apt-get dist-upgrade -y && \
    apt-get clean

# Install basic packages
RUN apt-get install -y curl erlang-base-hipe erlang-common-test erlang-dev \
                       erlang-eunit erlang-nox git libprotobuf9 libtbb2 \
                       openssl python rsync unzip && \
    apt-get clean

# Set up the environment
ENV HOME /root
