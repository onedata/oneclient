# bamboos
*bamboos* is a set of python scripts that are used for starting components in dockerized environment. Environment description files (JSON format) are used to configure how components should be started.

# User Guide
*bamboos* scripts can be used in two ways:

- **standalone scripts**
Navigate to use it as a standalone scripts `docker` directory and use any `*_up.py` script.

- **python library**
In your own script import `*.py` files located in `docker/environment` directory.

Consult those scripts for usage instructions. They require a JSON describing the environment to be started. See *example_env* dir for exemplary JSON files.  

Below is an example of JSON that defines a *oneprovider* instance, *onezone* instance and pre-configured entities like users and spaces. It can be used with:

 * `provider_up.py` script - starts *oneprovider*
 * `zone_up.py` script - starts *onezone*
 * `env_up.py` script - starts both *oneprovider* with *onezone* and pre-sets entities (users, spaces and groups)

The JSON looks like follows:
```json
{
    "dirs_config":{
        "cluster_manager":{
            "input_dir":"rel/cluster_manager",
            "target_dir":"rel/test_cluster"
        },
        "op_worker":{
            "input_dir":"rel/op_worker",
            "target_dir":"rel/test_cluster"
        },
        "oz_worker":{
            "input_dir":"rel/oz_worker",
            "target_dir":"rel/test_cluster"
        }
    },
    "os_configs":{
        "cfg1":{
            "storages":[
                {
                    "type":"posix",
                    "name":"/mnt/st1"
                }
            ],
            "users":[
                "user1"
            ],
            "groups":{
                "group1":[
                    "user1"
                ]
            }
        }
    },
    "provider_domains":{
        "p1":{
            "db_driver":"couchdb",
            "os_config":"cfg1",
            "cluster_manager":{
                "cm1":{
                    "vm.args":{
                        "setcookie":"cookie1"
                    },
                    "sys.config":{
                        "cluster_manager":{
                            "cm_nodes":[
                                "cm1"
                            ],
                            "worker_num":1
                        }
                    }
                }
            },
            "op_worker":{
                "worker1":{
                    "vm.args":{
                        "setcookie":"cookie1"
                    },
                    "sys.config":{
                        "op_worker":{
                            "cm_nodes":[
                                "cm1"
                            ],
                            "db_nodes":[
                                "dbnode1"
                            ],
                            "verify_oz_cert":false,
                            "oz_domain":"oz"
                        }
                    }
                }
            }
        }
    },
    "zone_domains":{
        "oz":{
            "db_driver":"couchdb",
            "cluster_manager":{
                "cm":{
                    "vm.args":{
                        "setcookie":"cookie3"
                    },
                    "sys.config":{
                        "cluster_manager":{
                            "cm_nodes":[
                                "cm"
                            ],
                            "worker_num":1
                        }
                    }
                }
            },
            "oz_worker":{
                "node1":{
                    "vm.args":{
                        "setcookie":"cookie3"
                    },
                    "sys.config":{
                        "oz_worker":{
                            "cm_nodes":[
                                "cm"
                            ],
                            "db_nodes":[
                                "127.0.0.1:49161"
                            ],
                            "http_domain":{
                                "string":"127.0.0.1"
                            },
                            "dev_mode":true
                        }
                    }
                }
            }
        }
    },
    "global_setup":{
        "users":{
            "user1":{
                "default_space":"space1"
            }
        },
        "groups":{
            "group1":{
                "users":[
                    "user1"
                ]
            }
        },
        "spaces":{
            "space1":{
                "displayed_name":"space1",
                "users":[
                    "user1"
                ],
                "groups":[
                    "group1"
                ],
                "providers":{
                    "p1":{
                        "storage":"/mnt/st1",
                        "supported_size":1000000000
                    }
                }
            }
        }
    }
}
```

Section **dirs_config** allows to define where to look for binaries of a given component and where to copy them to create different instances. Unless you really know what you are doing it should not be changed.

Section **os_configs** defines named configurations of Operating System, i.e. users, groups and storages to be created. Later, such config can be specified in [oneprovider](https://github.com/onedata/op-worker) configuration which will create required entites on every docker hosting the *oneprovider* nodes.

Section **provider_domains** defines a list of *oneprovider* instances. Each instance must get its unique identifier (e.g. `p1`), which will be transformed to domain name like this: `p1.1465312143.dev`. Inside provider configuration, you can specify which database driver and predefined *os_config* it should use, as well as how many nodes should be set up into a cluster. Each node can have its own configuration.

Section **zone_domains** defines a list of *onezone* instances. The configuration is simillar to *oneprovider* config, excluding *os_config* which is not used in onezone.

Section **global_setup** defines what entities should be initialized in *onezone*. This enables automatic creation of users, spaces, groups and provider supports. They will be all running and ready to use when script finishes.

# bamboos in Onedata
**bamboos** is used for test automation by setting up testing environments. It is used in [bamboo](https://www.atlassian.com/software/bamboo) builds, as well as in standaalone tests on developer machines. In addition, it allows for easy setup of dockerized environment which is useful during manual testing and code development.

