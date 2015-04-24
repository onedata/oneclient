// Author: Konrad Zemek
// Copyright (C) 2015 ACK CYFRONET AGH
// This software is released under the MIT license cited in 'LICENSE.txt'
//
// createService is a plugin for SkyDock that customizes the way container
// data is translated to a DNS entry.

function createService(container) {
    return {
        Port: 80,
        Environment: defaultEnvironment,
        TTL: defaultTTL,
        Service: container.Name.split('_').slice(-1),
        Instance: removeSlash(container.Name.split('_').slice(0, -1).join('_')),
        Host: container.NetworkSettings.IpAddress
    };
}
