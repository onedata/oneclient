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
#include "helpers/IStorageHelper.hh"

/**
 * Singleton Factory providing objects of requested storage helpers.
 */
class StorageHelperFactory {
	private:
        static StorageHelperFactory *_instance;     ///< Singleton instance.

		StorageHelperFactory();

	public:
        static StorageHelperFactory *instance();    ///< Singleton instance getter.

		~StorageHelperFactory();	

        /**
         * Produces storage helper object.
         * @param sh Name of storage helper that has to be returned.
         * @param args Arguments vector passed as argument to storge helper's constructor.
         * @return Pointer to storage helper object along with its ownership.
         */
        IStorageHelper* getStorageHelper(std::string sh, std::vector<std::string> args);
	
};

#endif // STORAGE_HELPER_FACTORY_HH
