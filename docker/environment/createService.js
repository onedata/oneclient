// createService is a plugin for SkyDock that customizes the way container
// data is translated to a DNS entry.
function createService(container) {
    return {
        Port: 80,
        Environment: defaultEnvironment,
        TTL: defaultTTL,
        Service: container.Name.split('_')[1],
        Instance: removeSlash(container.Name.split('_')[0]),
        Host: container.NetworkSettings.IpAddress
    }; 
}