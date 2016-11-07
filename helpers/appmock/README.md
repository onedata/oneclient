# appmock
*appmock* allows you to mock a service that exposes REST or TPC interfaces.

# User Guide
## Dependencies

* docker client > 1.10
* python >= 2.7

## Building
To build *appmock* use `./make.py`.

## Configuration and Running
*appmock* can be started using [bamboo](https://github.com/onedata/bamboo) scripts that are included in the repository. From the root of *appmock* project, run:

```
./bamboos/docker/appmock_up.py bamboos/example_env/appmock_example.json
```

The `appmock_up.py` script requires a JSON with configuration - simplest one can be found in `bamboos/example_env/appmock_example.json`.
 
The JSON config includes an entry called `app_description_file.json`. Its value should contain path to configuration file (in Erlang) that will be used to expose required endpoints. The path can be relative (to the JSON file) or absolute.

Example app description file can be found in `bamboos/example_env/example_app_description.erl`.
It contains exhaustive information on all functionalities that can be used.

Consult the script and the JSON file for more info.

# APIs
*appmock* has a remote control interface which allows for validating mocked endpoints and more.

There are two API clients:
* [appmock_client.py](https://github.com/onedata/appmock/blob/master/appmock_client.py)
* [appmock_client.erl](https://github.com/onedata/appmock/blob/master/src/client/appmock_client.erl)

that can be used as standalone scripts or included as libraries. Consult them for more information.

# appmock in Onedata
*appmock* is used in onedata during integration and acceptance tests.
