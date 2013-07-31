#ifndef STORAGE_HELPER_FACTORY_HH
#define STORAGE_HELPER_FACTORY_HH 1

#include <memory>
#include <string>
#include "helpers/IStorageHelper.hh"

class StorageHelperFactory {
	private:
		static StorageHelperFactory *_instance;

		StorageHelperFactory();

	public:
		static StorageHelperFactory *instance();

		~StorageHelperFactory();	

		std::shared_ptr<IStorageHelper> getStorageHelper(std::string, std::string);	
	
};

#endif // STORAGE_HELPER_FACTORY_HH