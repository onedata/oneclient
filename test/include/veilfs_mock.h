/**
 * @file veilfs_mock.h
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef VEILFS_MOCK_H
#define VEILFS_MOCK_H

#include "veilfs.h"
#include "testCommon.h"

class VeilFSMock {
public:
    VeilFSMock(){}
    ~VeilFSMock(){}

    MOCK_METHOD0(getScheduler, std::shared_ptr<JobScheduler>());
};

#endif // VEILFS_MOCK_H
