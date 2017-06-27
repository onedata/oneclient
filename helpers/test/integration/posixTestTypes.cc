/**
 * @file posixTestTypes.cc
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */
#include "helpers/storageHelper.h"

#include <string>
#include <sys/stat.h>
#include <vector>

#include <boost/make_shared.hpp>
#include <boost/python.hpp>
#include <boost/python/extract.hpp>
#include <boost/python/raw_function.hpp>
#include <boost/python/suite/indexing/vector_indexing_suite.hpp>

using namespace boost::python;
using namespace one::helpers;

using ReadDirResult = std::vector<std::string>;
using FlagsVector = std::vector<Flag>;

std::vector<Flag> maskToFlagsV(int mask) {
    auto flagset = maskToFlags(mask);
    return std::vector<Flag>(flagset.begin(), flagset.end());
}

BOOST_PYTHON_MODULE(posix_test_types)
{
    class_<ReadDirResult>("ReadDirResult")
        .def(vector_indexing_suite<ReadDirResult>());

    class_<std::vector<Flag>>("FlagsSet")
        .def(vector_indexing_suite<std::vector<Flag>>());

    class_<struct stat>("stat")
        .def_readwrite("st_dev", &stat::st_dev)
        .def_readwrite("st_ino", &stat::st_ino)
        .def_readwrite("st_mode", &stat::st_mode)
        .def_readwrite("st_nlink", &stat::st_nlink)
        .def_readwrite("st_uid", &stat::st_uid)
        .def_readwrite("st_gid", &stat::st_gid)
        .def_readwrite("st_rdev", &stat::st_rdev)
        .def_readwrite("st_size", &stat::st_size)
        .def_readwrite("st_blksize", &stat::st_blksize)
        .def_readwrite("st_blocks", &stat::st_blocks);

    enum_<Flag>("Flag")
        .value("NONBLOCK", Flag::NONBLOCK)
        .value("APPEND", Flag::APPEND)
        .value("ASYNC", Flag::ASYNC)
        .value("FSYNC", Flag::FSYNC)
        .value("NOFOLLOW", Flag::NOFOLLOW)
        .value("CREAT", Flag::CREAT)
        .value("TRUNC", Flag::TRUNC)
        .value("EXCL", Flag::EXCL)
        .value("RDONLY", Flag::RDONLY)
        .value("WRONLY", Flag::WRONLY)
        .value("RDWR", Flag::RDWR)
        .value("IFREG", Flag::IFREG)
        .value("IFCHR", Flag::IFCHR)
        .value("IFBLK", Flag::IFBLK)
        .value("IFIFO", Flag::IFIFO)
        .value("IFSOCK", Flag::IFSOCK)
        .export_values();

    def("maskToFlags", maskToFlagsV);
}