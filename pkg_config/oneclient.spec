%global _scl_prefix /opt/onedata

%{?scl:%scl_package oneclient-base}
%{!?scl:%global pkg_name %{name}}

%global version {{version}}
%global folly_version 2017.10.02.00
%global xrootd_version 20200703

Name:		%{?scl_prefix}oneclient-base
Version:	%{version}
Release:	1%{?dist}
Summary:	FUSE client for Onedata distributed filesystem
Group:		Applications/File
License:	MIT
URL:		https://onedata.org
Source0:	oneclient-base-%{version}.orig.tar.gz

Requires: epel-release
Requires: fuse
Requires: xrootd-libs >= %{xrootd_version>
Requires: xrootd-client-libs >= %{xrootd_version}
Requires: xrootd-server-libs >= %{xrootd_version}
Requires: scl-utils
Requires: %scl_require_package %{scl} tbb >= 2018.5~
BuildRequires: %scl_require_package %{scl} aws-sdk-cpp-s3 >= 1.4.35
BuildRequires: %scl_require_package %{scl} boost-devel >= 1.58.0
BuildRequires: %scl_require_package %{scl} boost-python >= 1.58.0
BuildRequires: %scl_require_package %{scl} boost-python3 >= 1.58.0
BuildRequires: %scl_require_package %{scl} folly-devel = %{folly_version}
BuildRequires: %scl_require_package %{scl} folly-static = %{folly_version}
BuildRequires: %scl_require_package %{scl} gflags-devel >= 2.1.2
BuildRequires: %scl_require_package %{scl} glog-devel >= 0.3.4
BuildRequires: %scl_require_package %{scl} glusterfs-api-devel >= 3.12.15
BuildRequires: %scl_require_package %{scl} librados-devel
BuildRequires: %scl_require_package %{scl} libradospp-devel
BuildRequires: %scl_require_package %{scl} libradosstriper-devel
BuildRequires: %scl_require_package %{scl} poco-devel
BuildRequires: %scl_require_package %{scl} poco-foundation
BuildRequires: %scl_require_package %{scl} poco-netssl
BuildRequires: %scl_require_package %{scl} poco-util
BuildRequires: %scl_require_package %{scl} poco-xml
BuildRequires: %scl_require_package %{scl} protobuf-compiler >= 3.4.1
BuildRequires: %scl_require_package %{scl} protobuf-devel >= 3.4.1
BuildRequires: %scl_require_package %{scl} protobuf-static >= 3.4.1
BuildRequires: %scl_require_package %{scl} proxygen-devel = %{folly_version}
BuildRequires: %scl_require_package %{scl} proxygen-static = %{folly_version}
BuildRequires: %scl_require_package %{scl} swift-sdk-cpp >= 1.0.0
BuildRequires: %scl_require_package %{scl} tbb-devel >= 2018.5~
BuildRequires: %scl_require_package %{scl} wangle-devel = %{folly_version}
BuildRequires: %scl_require_package %{scl} wangle-static = %{folly_version}
BuildRequires: %scl_require_package devtoolset-7 gcc-c++
BuildRequires: binutils-devel
BuildRequires: cmake3
BuildRequires: double-conversion-devel
BuildRequires: epel-release
BuildRequires: fuse-devel >= 2.7
BuildRequires: git
BuildRequires: golang
BuildRequires: libcurl-devel
BuildRequires: libevent-devel
BuildRequires: libsodium-devel
BuildRequires: libtool-ltdl
BuildRequires: libtool-ltdl-devel
BuildRequires: nspr-devel
BuildRequires: nss-devel
BuildRequires: openssl >= 1.0.0
BuildRequires: openssl-devel >= 1.0.0
BuildRequires: xrootd-private-devel >= %{xrootd_version}
BuildRequires: xrootd-server-devel >= %{xrootd_version}
BuildRequires: xrootd-client-devel >= %{xrootd_version}
BuildRequires: xrootd-devel >= %{xrootd_version}
BuildRequires: python-devel
BuildRequires: python36-devel
BuildRequires: subversion

%description
Oneclient is a software based on FUSE (Filesystem in Userspace) that
allows mounting onedata filesystem on Linux systems.


%package -n %{?scl_prefix}python2-onedatafs
Summary: Python 2 OnedataFS library.
Requires: epel-release
Requires: python
Requires: scl-utils
Requires: xrootd-server-libs >= %{xrootd_version}
Requires: xrootd-client-libs >= %{xrootd_version}
Requires: xrootd-libs >= %{xrootd_version}
Requires: %scl_require_package %{scl} tbb >= 2018.5~
Requires: %scl_require_package %{scl} boost-python

%description -n %{?scl_prefix}python2-onedatafs
Python 2 OnedataFS library.


%package -n %{?scl_prefix}python3-onedatafs
Summary: Python 3 OnedataFS library.
Requires: epel-release
Requires: python36
Requires: scl-utils
Requires: xrootd-server-libs >= %{xrootd_version}
Requires: xrootd-client-libs >= %{xrootd_version}
Requires: xrootd-libs >= %{xrootd_version}
Requires: %scl_require_package %{scl} tbb >= 2018.5~
Requires: %scl_require_package %{scl} boost-python3

%description -n %{?scl_prefix}python3-onedatafs
Python 3 OnedataFS library.

%define _unpackaged_files_terminate_build 0

%prep
%setup -q -n oneclient-base-%{version}

%build
cat <<SCL_EOF_MACRO | scl enable devtoolset-7 %{scl} --
LDFLAGS="-L/opt/onedata/%{scl}/root/usr/lib64" \
CFLAGS="-I/opt/onedata/%{scl}/root/usr/include" \
CXXFLAGS="-I/opt/onedata/%{scl}/root/usr/include" \
TBB_INSTALL_DIR=/opt/onedata/%{scl}/root/usr \
cmake3 . -DLIB_INSTALL_DIR=lib64 \
         -DCMAKE_INSTALL_PREFIX=/usr \
         -DCMAKE_LIBRARY_PATH=/opt/onedata/%{scl}/root/usr/lib64 \
         -DCMAKE_INCLUDE_PATH=/opt/onedata/%{scl}/root/usr/include \
         -DGLOG_INCLUDE_DIR_HINTS=/opt/onedata/%{scl}/root/usr/include \
         -DGLOG_LIBRARY_DIR_HINTS=/opt/onedata/%{scl}/root/usr/lib64 \
         -DGFLAGS_INCLUDEDIR=/opt/onedata/%{scl}/root/usr/include \
         -DGFLAGS_LIBRARYDIR=/opt/onedata/%{scl}/root/usr/lib64 \
         -DFOLLY_INCLUDEDIR=/opt/onedata/%{scl}/root/usr/include \
         -DFOLLY_LIBRARYDIR=/opt/onedata/%{scl}/root/usr/lib64 \
         -DWANGLE_INCLUDE_DIR=/opt/onedata/%{scl}/root/usr/include \
         -DBOOST_ROOT=/opt/onedata/%{scl}/root/usr \
         -DProtobuf_INCLUDE_DIR=/opt/onedata/%{scl}/root/usr/include \
         -DWITH_CEPH=ON -DWITH_GLUSTERFS=ON -DWITH_SWIFT=ON -DWITH_WEBDAV=ON -DWITH_XROOTD=ON \
         -DCMAKE_BUILD_TYPE=Release -DBUILD_INTEGRATION_TESTS=OFF -DBUILD_SHARED_LIBS=ON \
         -DSTATIC_LIBSTDCPP=ON -DSTATIC_BOOST=OFF -DSTATIC_PROTOBUF=ON
make %{_smp_mflags} oneclient
make %{_smp_mflags} onebench
make %{_smp_mflags} onedatafs.py2
make %{_smp_mflags} onedatafs.py3
SCL_EOF_MACRO

%install
rm -rf $RPM_BUILD_ROOT
make install DESTDIR=$RPM_BUILD_ROOT/opt/onedata/%{scl}/root/

%files
%{_bindir}/oneclient
%{_bindir}/onebench
/opt/onedata/%{scl}/root/etc/oneclient.conf
%{_mandir}/man1/oneclient.1.gz
%{_mandir}/man5/oneclient.conf.5.gz
/opt/onedata/%{scl}/root/var/lib/oneclient/*

%license
%{_defaultdocdir}/oneclient/LICENSE.txt

%doc
%{_defaultdocdir}/oneclient/README.md

%files -n %{?scl_prefix}python2-onedatafs
/opt/onedata/%{scl}/root/%{python_sitearch}/onedatafs/*

%post -n %{?scl_prefix}python2-onedatafs
%{__ln_s} -f /opt/onedata/%{scl}/root/%{python_sitearch}/onedatafs/onedatafs_py2.so /opt/onedata/%{scl}/root/%{python_sitearch}/onedatafs/onedatafs.so
%{__ln_s} -f /opt/onedata/%{scl}/root/usr/lib64/ceph/libceph-common.so.0 /opt/onedata/%{scl}/root/usr/lib64/libceph-common.so.0

%files -n %{?scl_prefix}python3-onedatafs
/opt/onedata/%{scl}/root/%{python3_sitearch}/onedatafs/*

%post -n %{?scl_prefix}python3-onedatafs
%{__ln_s} -f /opt/onedata/%{scl}/root/%{python3_sitearch}/onedatafs/onedatafs_py3.so /opt/onedata/%{scl}/root/%{python3_sitearch}/onedatafs/onedatafs.so
%{__ln_s} -f /opt/onedata/%{scl}/root/usr/lib64/ceph/libceph-common.so.0 /opt/onedata/%{scl}/root/usr/lib64/libceph-common.so.0

%changelog
* %(date +"%a %b %d %Y") Onedata Package Maintainer <support@onedata.org>
  - Build from %{version}
