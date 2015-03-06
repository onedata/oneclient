FROM onedata/base:v2
MAINTAINER Konrad Zemek <konrad.zemek@gmail.com>

# Add a recent docker binary
ADD https://get.docker.com/builds/Linux/x86_64/docker-latest /usr/bin/docker
RUN chmod a+x /usr/bin/docker

EXPOSE 53 5555 443 8443 80 8876 8877
