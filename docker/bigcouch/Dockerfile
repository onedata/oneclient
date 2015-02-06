FROM ubuntu:10.04
MAINTAINER Konrad Zemek <konrad.zemek@gmail.com>

# Get the image up to date
RUN apt-get update && \
    apt-get dist-upgrade -y && \
    apt-get install -y lsb-release && \
    apt-get clean all

RUN echo "deb http://packages.cloudant.com/ubuntu `lsb_release -cs` main" >> /etc/apt/sources.list.d/cloudant.list
RUN apt-get update && \
    apt-get install -y --force-yes bigcouch && \
    apt-get clean all
