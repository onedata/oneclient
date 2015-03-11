FROM ubuntu:14.04.1
MAINTAINER Konrad Zemek <konrad.zemek@gmail.com>

# Get the image up to date
RUN apt-get update && \
    apt-get dist-upgrade -y && \
    apt-get clean

# Install Riak
RUN apt-get install -y apt-transport-https curl && \
    apt-get clean
RUN curl https://packagecloud.io/gpg.key | apt-key add -
RUN curl "https://packagecloud.io/install/repositories/basho/riak/config_file.list?os=ubuntu&dist=trusty" > /etc/apt/sources.list.d/basho.list
RUN apt-get update && \
    apt-get install riak && \
    apt-get clean
