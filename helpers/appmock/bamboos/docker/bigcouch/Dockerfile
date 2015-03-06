FROM ubuntu:10.04
MAINTAINER Konrad Zemek <konrad.zemek@gmail.com>

# Get the image up to date
RUN apt-get update && \
    apt-get dist-upgrade -y && \
    apt-get clean all

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 59E01FBD15BE8E26
RUN echo 'deb http://packages.cloudant.com/ubuntu lucid main' > /etc/apt/sources.list.d/cloudant.list
RUN apt-get update && \
    apt-get install -y bigcouch && \
    apt-get clean all
