FROM onedata/base:v2
MAINTAINER Konrad Zemek <konrad.zemek@gmail.com>

# Add custom package sources
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 828AB726
RUN echo 'deb http://ppa.launchpad.net/george-edison55/cmake-3.x/ubuntu utopic main' > /etc/apt/sources.list.d/cmake.list
ADD http://toolkit.globus.org/ftppub/gt6/installers/repo/globus-toolkit-repo_latest_all.deb /root/globus-toolkit-repo_latest_all.deb
RUN dpkg -i /root/globus-toolkit-repo_latest_all.deb

# Install development packages
RUN apt-get update && \
    apt-get install -y clang cmake doxygen elixir erlang erlang-src gdb \
                       libboost-all-dev libbotan1.10-dev libfuse-dev \
                       libglobus-common-dev libglobus-gsi-callback-dev \
                       libltdl-dev libprotobuf-dev libstdc++-4.9-dev \
                       libtbb-dev ninja-build pkg-config protobuf-compiler \
                       python-sphinx rpm subversion && \
    apt-get clean

# Set up the environment
ENV CC=clang CXX=clang++
RUN update-alternatives --set cc /usr/bin/clang && \
    update-alternatives --set c++ /usr/bin/clang++
RUN echo '    StrictHostKeyChecking no'     >> /etc/ssh/ssh_config && \
    echo '    UserKnownHostsFile /dev/null' >> /etc/ssh/ssh_config
