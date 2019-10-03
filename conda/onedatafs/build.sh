#!/bin/bash

mkdir build

cd build

export CXXFLAGS="-Wno-shadow -Wno-unused-local-typedefs -Wno-sign-compare -fvisibility-inlines-hidden -std=c++14 -fmessage-length=0 -march=nocona -mtune=haswell -ftree-vectorize -fPIC -fstack-protector-strong -fno-plt -O2 -ffunction-sections -pipe -I$BUILD_PREFIX/include -I$PREFIX/include -fdebug-prefix-map=$SRC_DIR=/usr/local/src/conda/oneclient-$ONECLIENT_VERSION -fdebug-prefix-map=$PREFIX=/usr/local/src/conda-prefix"

export PKG_CONFIG_PATH="$BUILD_PREFIX/lib/pkgconfig:$PKG_CONFIG_PATH"

if [[ ${CONDA_PY:0:1} == "2" ]]; then
    export WITH_PYTHON2=ON
    export WITH_PYTHON3=OFF
    echo "################ Building onedatafs for Python 2 ##################"
else
    export WITH_PYTHON2=OFF
    export WITH_PYTHON3=ON
    echo "################ Building onedatafs for Python 3 ##################"
fi

cmake -DCMAKE_BUILD_TYPE=Release \
	  -DGIT_VERSION=${PKG_VERSION} \
	  -DCODE_COVERAGE=OFF \
	  -DWITH_CEPH=ON \
	  -DWITH_SWIFT=ON \
      -DCMAKE_AR=${AR} \
	  -DWITH_S3=ON \
	  -DWITH_GLUSTERFS=ON \
	  -DWITH_WEBDAV=ON \
	  -DWITH_ONECLIENT=OFF \
	  -DWITH_ONEBENCH=OFF \
	  -DWITH_ONEDATAFS=ON \
	  -DFOLLY_SHARED=ON \
	  -DWITH_LIBDL=OFF \
	  -DWITH_LIBRT=OFF \
	  -DWITH_TESTS=OFF \
      -DWITH_PYTHON2=${WITH_PYTHON2} \
	  -DWITH_PYTHON3=${WITH_PYTHON3} \
	  -DBoost_NO_BOOST_CMAKE=ON \
	  -DCMAKE_INSTALL_PREFIX=${PREFIX} \
	  ..

make -j${CPU_COUNT} onedatafs.py${CONDA_PY:0:1} VERBOSE=1

make install

ONEDATAFS_PREFIX=$PREFIX/lib/python${CONDA_PY:0:1}.${CONDA_PY:1:1}/site-packages/onedatafs

ln -s ${ONEDATAFS_PREFIX}/onedatafs_py${CONDA_PY:0:1}.so ${ONEDATAFS_PREFIX}/onedatafs.so
