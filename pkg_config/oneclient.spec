%global version {{version}}

Name:		oneclient
Version:	%{version}
Release:	{{build}}%{?dist}
Summary:	FUSE client for onedata system
Group:		Applications/File
License:	MIT
URL:		https://onedata.org
Source0:	oneclient-%{version}.orig.tar.gz

Requires: fuse
BuildRequires: aws-sdk-cpp-s3 >= 1.0.11
BuildRequires: binutils-devel,
BuildRequires: boost-devel >= 1.58.0
BuildRequires: cmake >= 3.0.0
BuildRequires: double-conversion-devel
BuildRequires: fuse-devel >= 2.7
BuildRequires: folly-devel = 2016.09.19.00, folly-static = 2016.09.19.00
BuildRequires: gcc-c++ >= 5.0.0
BuildRequires: git
BuildRequires: glog-devel >= 0.3.4
BuildRequires: glusterfs-api-devel = 3.7.18
BuildRequires: golang
BuildRequires: libcurl-devel
BuildRequires: librados-devel >= 11.1.0
BuildRequires: libsodium-devel
BuildRequires: libtool-ltdl
BuildRequires: libtool-ltdl-devel
BuildRequires: nspr-devel
BuildRequires: nss-devel
BuildRequires: openssl
BuildRequires: poco-devel
BuildRequires: poco-foundation
BuildRequires: poco-netssl
BuildRequires: poco-util
BuildRequires: poco-xml
BuildRequires: openssl-devel
BuildRequires: protobuf-compiler >= 2.6.0
BuildRequires: protobuf-devel >= 2.6.0
BuildRequires: python-devel
BuildRequires: subversion
BuildRequires: swift-sdk-cpp >= 1.0.0
BuildRequires: tbb-devel >= 4.3~

%description
oneclient is a software based on FUSE (Filesystem in Userspace) that allows mounting onedata filesystem on Linux systems.

%prep
%setup -q

%build
%cmake . -DCMAKE_BUILD_TYPE=Release -DBUILD_INTEGRATION_TESTS=Off -DBUILD_SHARED_LIBS=Off
make %{?_smp_mflags} oneclient

%install
make install DESTDIR=%{buildroot}

%files
%{_bindir}/oneclient
%{_sysconfdir}/oneclient.conf
%{_mandir}/man1/oneclient.1.gz
%{_mandir}/man5/oneclient.conf.5.gz
%{_localstatedir}/lib/oneclient/*

%license
%{_defaultdocdir}/oneclient/LICENSE.txt

%doc
%{_defaultdocdir}/oneclient/README.md

%changelog
* %(date +"%a %b %d %Y") Onedata Package Maintainer <support@onedata.org>
  - Build from %{version}
