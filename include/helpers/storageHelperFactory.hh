/**
 * @file storageHelperFactory.hh
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef STORAGE_HELPER_FACTORY_HH
#define STORAGE_HELPER_FACTORY_HH 1

#include <memory>
#include <string>
#include <vector>
#include <boost/shared_ptr.hpp>
#include "helpers/IStorageHelper.hh"

using namespace std;
using namespace boost;

/**
 * Factory providing objects of requested storage helpers.
 */
class StorageHelperFactory {

	public:
        StorageHelperFactory();
		virtual ~StorageHelperFactory();	

        /**
         * Produces storage helper object.
         * @param sh Name of storage helper that has to be returned.
         * @param args Arguments vector passed as argument to storge helper's constructor.
         * @return Pointer to storage helper object along with its ownership.
         */
        virtual shared_ptr<IStorageHelper> getStorageHelper(string sh, vector<string> args);
	
};

#endif // STORAGE_HELPER_FACTORY_HH
