#!/bin/bash

mkdir build

cd build

export CXXFLAGS="-Wno-shadow -Wno-unused-local-typedefs -Wno-sign-compare -fvisibility-inlines-hidden -std=c++14 -fmessage-length=0 -march=nocona -mtune=haswell -ftree-vectorize -fPIC -fstack-protector-strong -fno-plt -O2 -ffunction-sections -pipe -I$BUILD_PREFIX/include -I$PREFIX/include -fdebug-prefix-map=$SRC_DIR=/usr/local/src/conda/oneclient-$ONECLIENT_VERSION -fdebug-prefix-map=$PREFIX=/usr/local/src/conda-prefix"

export PKG_CONFIG_PATH="$BUILD_PREFIX/lib/pkgconfig:$PKG_CONFIG_PATH"

cmake -DCMAKE_BUILD_TYPE=Release \
	  -DGIT_VERSION=${PKG_REVISION} \
	  -DCODE_COVERAGE=OFF \
	  -DWITH_CEPH=ON \
	  -DWITH_SWIFT=ON \
      -DCMAKE_AR=${AR} \
	  -DWITH_S3=ON \
	  -DWITH_GLUSTERFS=ON \
	  -DWITH_WEBDAV=ON \
	  -DWITH_ONECLIENT=ON \
	  -DWITH_ONEBENCH=ON \
	  -DWITH_ONEDATAFS=OFF \
	  -DFOLLY_SHARED=ON \
	  -DWITH_LIBDL=OFF \
	  -DWITH_LIBRT=OFF \
	  -DWITH_TESTS=OFF \
	  -DBoost_NO_BOOST_CMAKE=ON \
	  -DCMAKE_INSTALL_PREFIX=${PREFIX} \
	  ..

make -j${CPU_COUNT} oneclient VERBOSE=1

make -j${CPU_COUNT} onebench VERBOSE=1

make install

