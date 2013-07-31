/**
 * @file storageHelperFactory.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "helpers/storageHelperFactory.hh"
#include "helpers/directIOHelper.hh"
#include "glog/logging.h"

StorageHelperFactory* StorageHelperFactory::_instance = NULL;

StorageHelperFactory::StorageHelperFactory() 
{
}

StorageHelperFactory* StorageHelperFactory::instance() {
	if(_instance == NULL) {
		_instance = new StorageHelperFactory();
	}
	return _instance;
}


IStorageHelper* StorageHelperFactory::getStorageHelper(std::string sh_name, std::vector<std::string> args) {
    if(sh_name == "DirectIO")
        return new DirectIOHelper(args);
    else
    {
        LOG(ERROR) << "Unknown storage helper: " << sh_name;
        return NULL;
    }
}
